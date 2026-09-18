#include "raylib.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "lib/nes.h"
#include "lib/input_trace.h"
#include "frame_pacing.h"

#define SCALE_FACTOR 3
#define WINDOW_WIDTH (SCREEN_WIDTH * SCALE_FACTOR)
#define WINDOW_HEIGHT (SCREEN_HEIGHT * SCALE_FACTOR)
#define AUDIO_SAMPLE_RATE 44100
#define AUDIO_STREAM_BUFFER_SIZE 512
#define AUDIO_BUFFER_TARGET 2048

#define CONTROLLER_RIGHT 0b10000000
#define CONTROLLER_LEFT 0b01000000
#define CONTROLLER_DOWN 0b00100000
#define CONTROLLER_UP 0b00010000
#define CONTROLLER_START 0b00001000
#define CONTROLLER_SELECT 0b00000100
#define CONTROLLER_B 0b00000010
#define CONTROLLER_A 0b00000001

#define CONTROLLER1_UP_KEY KEY_W
#define CONTROLLER1_LEFT_KEY KEY_A
#define CONTROLLER1_DOWN_KEY KEY_S
#define CONTROLLER1_RIGHT_KEY KEY_D
#define CONTROLLER1_A_KEY KEY_L
#define CONTROLLER1_B_KEY KEY_K
#define CONTROLLER1_START_KEY KEY_ENTER
#define CONTROLLER1_SELECT_KEY KEY_SPACE

u8 read_controller1_state(void) {
    u8 state = 0;

    // Keyboard inputs
    if (IsKeyDown(CONTROLLER1_UP_KEY)) state |= CONTROLLER_UP;
    if (IsKeyDown(CONTROLLER1_LEFT_KEY)) state |= CONTROLLER_LEFT;
    if (IsKeyDown(CONTROLLER1_DOWN_KEY)) state |= CONTROLLER_DOWN;
    if (IsKeyDown(CONTROLLER1_RIGHT_KEY)) state |= CONTROLLER_RIGHT;
    if (IsKeyDown(CONTROLLER1_A_KEY)) state |= CONTROLLER_A;
    if (IsKeyDown(CONTROLLER1_B_KEY)) state |= CONTROLLER_B;
    if (IsKeyDown(CONTROLLER1_START_KEY)) state |= CONTROLLER_START;
    if (IsKeyDown(CONTROLLER1_SELECT_KEY)) state |= CONTROLLER_SELECT;

    return state;
}

static void step_frame_with_input(NES* nes, u8 state, InputTraceWriter* writer, bool* recording) {
    if (*recording) {
        char error[256];
        if (!input_trace_writer_write_frame(writer, state, error, sizeof(error))) {
            fprintf(stderr, "Input recording stopped: %s\n", error);
            input_trace_writer_close(writer, NULL, 0);
            *recording = false;
        }
    }

    cpu_update_controller1(&nes->cpu, state);
    nes_step_frame(nes);
}

APU* apu_instance = NULL;

void audio_input_callback(void* output_buffer, unsigned int frames) {
    f32 *samples = (f32*)output_buffer;
    apu_fill_buffer(apu_instance, samples, (usize)frames);
}

int main(int argc, char* argv[]) {
    const char* rom_path = NULL;
    const char* trace_path = NULL;
    bool pacing_stats = false;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--record") == 0) {
            if (trace_path != NULL || i + 1 >= argc) {
                fprintf(stderr, "Usage: %s [--record <trace_path>] [--pacing-stats] <rom_path>\n", argv[0]);
                return 1;
            }
            trace_path = argv[++i];
        } else if (strcmp(argv[i], "--pacing-stats") == 0) {
            pacing_stats = true;
        } else if (strcmp(argv[i], "--help") == 0) {
            printf("Usage: %s [--record <trace_path>] [--pacing-stats] <rom_path>\n", argv[0]);
            return 0;
        } else if (rom_path == NULL) {
            rom_path = argv[i];
        } else {
            fprintf(stderr, "Usage: %s [--record <trace_path>] [--pacing-stats] <rom_path>\n", argv[0]);
            return 1;
        }
    }

    if (rom_path == NULL) {
        fprintf(stderr, "Usage: %s [--record <trace_path>] [--pacing-stats] <rom_path>\n", argv[0]);
        return 1;
    }

    NES nes = {0};
    Result nes_init_res = nes_init_from_file(&nes, rom_path, AUDIO_SAMPLE_RATE);

    if (!nes_init_res.ok) {
        fprintf(stderr, "Error: %s\n", nes_init_res.error);
        return 1;
    }

    InputTraceWriter trace_writer = {0};
    bool recording = false;
    if (trace_path != NULL) {
        char error[256];
        if (!input_trace_writer_open(&trace_writer, trace_path, error, sizeof(error))) {
            fprintf(stderr, "Could not start input recording: %s\n", error);
            nes_free(&nes);
            return 1;
        }
        recording = true;
    }

    apu_instance = &nes.apu;

    SetConfigFlags(FLAG_VSYNC_HINT | FLAG_WINDOW_HIGHDPI);
    InitWindow(WINDOW_WIDTH, WINDOW_HEIGHT, "nesc");
    // EndDrawing waits for vsync. A second FPS limiter can miss the next swap.
    SetTargetFPS(0);

    SetAudioStreamBufferSizeDefault(AUDIO_STREAM_BUFFER_SIZE);

    InitAudioDevice();
    AudioStream stream = LoadAudioStream(AUDIO_SAMPLE_RATE, 32, 1);
    SetAudioStreamCallback(stream, audio_input_callback);

    while (apu_buffered_samples(&nes.apu) < AUDIO_BUFFER_TARGET) {
        // Capture startup audio priming as neutral input frames so replay
        // begins from the same emulator state as the recorded session.
        step_frame_with_input(&nes, 0, &trace_writer, &recording);
    }

    Image image = {
        .data = nes.ppu.frame,
        .width = SCREEN_WIDTH,
        .height = SCREEN_HEIGHT,
        .format = PIXELFORMAT_UNCOMPRESSED_R8G8B8A8,
        .mipmaps = 1,
    };

    Texture2D texture = LoadTextureFromImage(image);
    SetTextureFilter(texture, TEXTURE_FILTER_POINT);
    SetExitKey(KEY_NULL);
    SetWindowMaxSize(WINDOW_WIDTH, WINDOW_HEIGHT);
    SetWindowMinSize(WINDOW_HEIGHT, WINDOW_HEIGHT);

    Rectangle source = { 0, 0, SCREEN_WIDTH, SCREEN_HEIGHT };
    Rectangle dest = { 0, 0, WINDOW_WIDTH, WINDOW_HEIGHT };
    // Finish first-use graphics setup before starting the audio/video clocks.
    BeginDrawing();
        ClearBackground(WHITE);
        DrawTexturePro(texture, source, dest, (Vector2){ 0, 0 }, 0.0f, WHITE);
    EndDrawing();
    usize audio_underrun_samples = 0;
    usize presentations = 0, emulated_frames = 0, repeats = 0, catchups = 0;
    double previous_tick = GetTime();
    double diagnostic_start = previous_tick;
    double longest_tick = 0.0;
    FramePacing pacing;
    int display_hz = GetMonitorRefreshRate(GetCurrentMonitor());
    frame_pacing_init(&pacing, previous_tick, display_hz, AUDIO_BUFFER_TARGET);
    double applied_pitch = pacing.audio_pitch;
    SetAudioStreamPitch(stream, (float)applied_pitch);
    PlayAudioStream(stream);
    if (pacing_stats) {
        fprintf(stderr, "Pacing: display %d Hz, emulation %.4f Hz, vsync enabled\n", display_hz, pacing.frame_hz);
    }

    while (!WindowShouldClose()) {
        double now = GetTime();
        double elapsed = now - previous_tick;
        previous_tick = now;
        if (elapsed > longest_tick) longest_tick = elapsed;
        u8 input_state = read_controller1_state();
        unsigned int frames = frame_pacing_advance(&pacing, now);
        for (unsigned int frame = 0; frame < frames; frame++) {
            step_frame_with_input(&nes, input_state, &trace_writer, &recording);
        }
        emulated_frames += frames;
        repeats += frames == 0;
        catchups += frames > 1;
        presentations++;

        if (frames > 0) UpdateTexture(texture, nes.ppu.frame);
        double pitch = frame_pacing_audio_pitch(&pacing, (double)apu_buffered_samples(&nes.apu),
                                                AUDIO_BUFFER_TARGET, elapsed);
        if (fabs(pitch - applied_pitch) >= 0.00001) {
            SetAudioStreamPitch(stream, (float)pitch);
            applied_pitch = pitch;
        }

        BeginDrawing();
            ClearBackground(WHITE);
            DrawTexturePro(texture, source, dest, (Vector2){ 0, 0 }, 0.0f, WHITE);
        EndDrawing();

        audio_underrun_samples += apu_take_underrun_samples(&nes.apu);
        double diagnostic_end = GetTime();
        if (diagnostic_end - diagnostic_start >= 1.0) {
            double seconds = diagnostic_end - diagnostic_start;
            if (pacing_stats) {
                fprintf(stderr, "Pacing: %.1f draws/s, %.1f emulated/s, repeats %zu, catchups %zu, "
                        "max interval %.2f ms, queue %zu, pitch %.5f, underrun %zu, resyncs %u\n",
                        (double)presentations / seconds, (double)emulated_frames / seconds,
                        repeats, catchups, longest_tick * 1000.0, apu_buffered_samples(&nes.apu),
                        applied_pitch, audio_underrun_samples, pacing.resyncs);
            } else if (audio_underrun_samples > 0) {
                fprintf(stderr, "Audio underrun: %zu samples in the last second\n", audio_underrun_samples);
            }
            audio_underrun_samples = 0;
            presentations = emulated_frames = repeats = catchups = 0;
            longest_tick = 0.0;
            diagnostic_start = diagnostic_end;
        }
    }

    StopAudioStream(stream);
    UnloadAudioStream(stream);
    UnloadTexture(texture);
    CloseAudioDevice();
    CloseWindow();

    if (recording) {
        char error[256];
        if (input_trace_writer_close(&trace_writer, error, sizeof(error))) {
            fprintf(stderr, "Recorded %zu emulated frames to %s\n", trace_writer.frame_count, trace_path);
        } else {
            fprintf(stderr, "Could not finish input recording: %s\n", error);
        }
    }

    nes_free(&nes);
    apu_instance = NULL;

    return 0;
}
