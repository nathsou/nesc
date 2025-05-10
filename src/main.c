#include "raylib.h"
#include <stdio.h>
#include <stdlib.h>
#include "nes.h"

#define SCALE_FACTOR 3
#define WINDOW_WIDTH (SCREEN_WIDTH * SCALE_FACTOR)
#define WINDOW_HEIGHT (SCREEN_HEIGHT * SCALE_FACTOR)

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

void handle_inputs(CPU* cpu, int gamepad) {
    bool up = false;
    bool down = false;
    bool left = false;
    bool right = false;
    bool a = false;
    bool b = false;
    bool start = false;
    bool select = false;
    
    // Keyboard inputs
    if (IsKeyDown(CONTROLLER1_UP_KEY)) up = true;
    if (IsKeyDown(CONTROLLER1_LEFT_KEY)) left = true;
    if (IsKeyDown(CONTROLLER1_DOWN_KEY)) down = true;
    if (IsKeyDown(CONTROLLER1_RIGHT_KEY)) right = true;
    if (IsKeyDown(CONTROLLER1_A_KEY)) a = true;
    if (IsKeyDown(CONTROLLER1_B_KEY)) b = true;
    if (IsKeyDown(CONTROLLER1_START_KEY)) start = true;
    if (IsKeyDown(CONTROLLER1_SELECT_KEY)) select = true;

    u8 state = 0;
    if (up) state |= CONTROLLER_UP;
    if (down) state |= CONTROLLER_DOWN;
    if (left) state |= CONTROLLER_LEFT;
    if (right) state |= CONTROLLER_RIGHT;
    if (a) state |= CONTROLLER_A;
    if (b) state |= CONTROLLER_B;
    if (start) state |= CONTROLLER_START;
    if (select) state |= CONTROLLER_SELECT;

    cpu_update_controller1(cpu, state);
}

APU* apu_instance = NULL;

void audio_input_callback(void* output_buffer, unsigned int frames) {
    u8 *samples = (u8*)output_buffer;
    apu_fill_buffer(apu_instance, samples, (usize)frames);
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        printf("Usage: %s <rom_path>\n", argv[0]);
        return 1;
    }

    NES nes;
    nes_init(&nes, argv[1]);
    apu_instance = &nes.apu;

    SetTargetFPS(60);
    SetConfigFlags(FLAG_VSYNC_HINT);
    InitWindow(WINDOW_WIDTH, WINDOW_HEIGHT, "nesc");

    SetAudioStreamBufferSizeDefault(128);

    InitAudioDevice();
    AudioStream stream = LoadAudioStream(AUDIO_SAMPLE_RATE, 8, 1);
    SetAudioStreamCallback(stream, audio_input_callback);
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
    
    int gamepad = 0;

    while (!WindowShouldClose()) {
        handle_inputs(&nes.cpu, gamepad);
        nes_step_frame(&nes);
        UpdateTexture(texture, nes.ppu.frame);

        BeginDrawing();
            ClearBackground(WHITE);
            DrawTexturePro(texture, source, dest, (Vector2){ 0, 0 }, 0.0f, WHITE);
        EndDrawing();
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
