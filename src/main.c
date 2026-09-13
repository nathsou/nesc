#include "raylib.h"
#include <stdio.h>
#include <stdlib.h>
#include "lib/nes.h"

#define SCALE_FACTOR 3
#define WINDOW_WIDTH (SCREEN_WIDTH * SCALE_FACTOR)
#define WINDOW_HEIGHT (SCREEN_HEIGHT * SCALE_FACTOR)
#define AUDIO_SAMPLE_RATE 44100
#define AUDIO_STREAM_BUFFER_SIZE 512
#define AUDIO_BUFFER_TARGET 2048
#define AUDIO_BUFFER_LOW_WATER (2 * AUDIO_STREAM_BUFFER_SIZE)

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

void handle_inputs(CPU* cpu) {
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

    cpu_update_controller1(cpu, state);
}

APU* apu_instance = NULL;

void audio_input_callback(void* output_buffer, unsigned int frames) {
    f32 *samples = (f32*)output_buffer;
    apu_fill_buffer(apu_instance, samples, (usize)frames);
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        printf("Usage: %s <rom_path>\n", argv[0]);
        return 1;
    }

    NES nes;
    Result nes_init_res = nes_init_from_file(&nes, argv[1], AUDIO_SAMPLE_RATE);

    if (!nes_init_res.ok) {
        fprintf(stderr, "Error: %s\n", nes_init_res.error);
        return 1;
    }

    apu_instance = &nes.apu;

    SetTargetFPS(60);
    SetConfigFlags(FLAG_VSYNC_HINT | FLAG_WINDOW_HIGHDPI);
    InitWindow(WINDOW_WIDTH, WINDOW_HEIGHT, "nesc");

    SetAudioStreamBufferSizeDefault(AUDIO_STREAM_BUFFER_SIZE);

    InitAudioDevice();
    AudioStream stream = LoadAudioStream(AUDIO_SAMPLE_RATE, 32, 1);
    SetAudioStreamCallback(stream, audio_input_callback);

    while (apu_buffered_samples(&nes.apu) < AUDIO_BUFFER_TARGET) {
        nes_step_frame(&nes);
    }

    PlayAudioStream(stream);

    Image image = {
        .data = nes.ppu.frame,
        .width = SCREEN_WIDTH,
        .height = SCREEN_HEIGHT,
        .format = PIXELFORMAT_UNCOMPRESSED_R8G8B8,
        .mipmaps = 1,
    };

    Texture2D texture = LoadTextureFromImage(image);
    SetTextureFilter(texture, TEXTURE_FILTER_POINT);
    SetExitKey(KEY_NULL);
    SetWindowMaxSize(WINDOW_WIDTH, WINDOW_HEIGHT);
    SetWindowMinSize(WINDOW_HEIGHT, WINDOW_HEIGHT);

    Rectangle source = { 0, 0, SCREEN_WIDTH, SCREEN_HEIGHT };
    Rectangle dest = { 0, 0, WINDOW_WIDTH, WINDOW_HEIGHT };
    usize audio_underrun_samples = 0;
    usize audio_diagnostic_frames = 0;

    while (!WindowShouldClose()) {
        handle_inputs(&nes.cpu);

        // Keep normal video pacing at one emulated frame per render tick.
        nes_step_frame(&nes);

        // A 60 Hz display consumes audio slightly faster than one NTSC NES
        // frame produces it. Run one occasional recovery frame before the
        // queue underruns, but never refill it with an unbounded frame burst.
        if (apu_buffered_samples(&nes.apu) < AUDIO_BUFFER_LOW_WATER) {
            nes_step_frame(&nes);
        }

        UpdateTexture(texture, nes.ppu.frame);

        BeginDrawing();
            ClearBackground(WHITE);
            DrawTexturePro(texture, source, dest, (Vector2){ 0, 0 }, 0.0f, WHITE);
        EndDrawing();

        audio_underrun_samples += apu_take_underrun_samples(&nes.apu);
        audio_diagnostic_frames++;
        if (audio_diagnostic_frames == 60) {
            if (audio_underrun_samples > 0) {
                fprintf(stderr, "Audio underrun: %zu samples in the last 60 frames\n", audio_underrun_samples);
            }
            audio_underrun_samples = 0;
            audio_diagnostic_frames = 0;
        }
    }

    StopAudioStream(stream);
    UnloadAudioStream(stream);
    UnloadTexture(texture);
    CloseAudioDevice();
    CloseWindow();
    nes_free(&nes);
    apu_instance = NULL;

    return 0;
}
