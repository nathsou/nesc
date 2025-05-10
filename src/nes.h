#ifndef NESC_NES_H
#define NESC_NES_H

#include "cpu.h"
#include "ppu.h"
#include "apu.h"
#include "cart.h"

#define AUDIO_SAMPLE_RATE 44100 // Hz
#define INES_HEADER_SIZE 16

typedef struct {
    CPU cpu;
    PPU ppu;
    APU apu;
    Cart cart;
} NES;

#endif

void nes_init(NES *nes, const char *rom_path);
void nes_step_frame(NES *nes);
void nes_free(NES *nes);
