#ifndef NESC_NES_H
#define NESC_NES_H

#include "cpu.h"
#include "ppu.h"
#include "apu.h"
#include "cart.h"
#include "mapper.h"

#define INES_HEADER_SIZE 16

typedef struct {
    CPU cpu;
    PPU ppu;
    APU apu;
    Cart cart;
    Mapper* mapper;
} NES;

typedef struct {
    bool ok;
    char error[256];
} Result;

Result nes_init(NES *nes, u8* rom_data, usize rom_size, usize audio_sample_rate);
Result nes_init_from_file(NES *nes, const char *rom_path, usize audio_sample_rate);
void nes_step_frame(NES *nes);
void nes_free(NES *nes);

#endif
