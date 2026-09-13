#include "nes.h"

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char* argv[]) {
    if (argc < 2 || argc > 3) {
        fprintf(stderr, "Usage: %s <rom_path> [max_frames]\n", argv[0]);
        return 2;
    }

    usize max_frames = argc == 3 ? (usize)strtoull(argv[2], NULL, 10) : 1800;
    NES nes = {0};
    Result result = nes_init_from_file(&nes, argv[1], 44100);
    if (!result.ok) {
        fprintf(stderr, "Failed to initialize ROM: %s\n", result.error);
        return 2;
    }

    bool identified = false;
    for (usize frame = 0; frame < max_frames; frame++) {
        nes_step_frame(&nes);
        u8 signature0 = cpu_read_byte(&nes.cpu, 0x6001);
        u8 signature1 = cpu_read_byte(&nes.cpu, 0x6002);
        u8 signature2 = cpu_read_byte(&nes.cpu, 0x6003);
        if (signature0 == 0xde && signature1 == 0xb0 && signature2 == 0x61) {
            identified = true;
            u8 status = cpu_read_byte(&nes.cpu, 0x6000);
            if (status < 0x80) {
                printf("status=%u after %zu frame(s)\n", status, frame + 1);
                nes_free(&nes);
                return status == 0 ? 0 : 1;
            }
        }
    }

    fprintf(stderr, identified ? "Test did not finish within %zu frames\n" : "ROM did not publish a Blargg test status\n", max_frames);
    nes_free(&nes);
    return 1;
}
