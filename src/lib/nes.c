#include "nes.h"
#include "nrom.h"
#include "mmc1.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

static inline Result result_ok() {
    return (Result){ .ok = true, .error = "\n" };
}

Result result_error(char* error, ...) {
    Result result = { .ok = false };
    va_list args;
    va_start(args, error);
    vsnprintf(result.error, sizeof(result.error), error, args);
    va_end(args);

    return result;
}

Mapper *get_mapper(INES ines) {
    switch (ines.mapper_type) {
        case 0: {
            Mapper_NROM* nrom = (Mapper_NROM*)malloc(sizeof(Mapper_NROM));
            mapper_nrom_init(nrom);
            return (Mapper*)nrom;
        }
        case 1: {
            Mapper_MMC1* mmc1 = (Mapper_MMC1*)malloc(sizeof(Mapper_MMC1));
            mapper_mmc1_init(mmc1);
            return (Mapper*)mmc1;
        }
    }

    return NULL;
}

Result nes_init(NES* nes, u8* rom_data, usize rom_size, usize audio_sample_rate) {
    usize rom_offset = 0;

    if (rom_size < INES_HEADER_SIZE) {
        return result_error("Invalid ROM size");
    }

    // read INES header
    u8 header[INES_HEADER_SIZE];
    memcpy(header, rom_data + rom_offset, INES_HEADER_SIZE);
    rom_offset += INES_HEADER_SIZE;
    INES ines = ines_parse(header);
    ines_print(ines);

    nes->mapper = get_mapper(ines);

    if (nes->mapper == NULL) {
        return result_error("Unsupported mapper: %d\n", ines.mapper_type);
    }

    LOG("ines version: %d\n", ines.is_ines_2);

    if (ines.trainer) {
        // skip trainer data
        rom_offset += 512;
    }

    // read PRG ROM
    usize prg_rom_size = ines.prg_banks * 16 * 1024;

    if (rom_size < rom_offset + prg_rom_size) {
        return result_error("Invalid PRG ROM size");
    }

    u8* prg_rom = (u8*)malloc(prg_rom_size);
    memcpy(prg_rom, rom_data + rom_offset, prg_rom_size);
    rom_offset += prg_rom_size;

    // read CHR ROM
    usize chr_rom_size = ines.chr_banks * 8 * 1024;

    if (rom_size < rom_offset + chr_rom_size) {
        free(prg_rom);
        return result_error("Invalid CHR ROM size");
    }

    u8* chr_rom = (u8*)malloc(chr_rom_size);
    memcpy(chr_rom, rom_data + rom_offset, chr_rom_size);
    rom_offset += chr_rom_size;

    nes->cart = cart_create(ines, prg_rom, prg_rom_size, chr_rom, chr_rom_size);
    nes->mapper->init(nes->mapper, &nes->cart);

    // initialize CPU, PPU and APU
    ppu_init(&nes->ppu, &nes->cart, nes->mapper);
    apu_init(&nes->apu, audio_sample_rate);
    cpu_init(&nes->cpu, &nes->ppu, &nes->apu, nes->mapper);

    return result_ok();
}

Result nes_init_from_file(NES* nes, const char* rom_path, usize audio_sample_rate) {
    FILE* file = fopen(rom_path, "rb");

    if (file == NULL) {
        fclose(file);
        return result_error("Failed to open ROM file '%s'", rom_path);
    }

    fseek(file, 0, SEEK_END);
    usize rom_size = (usize)ftell(file);
    fseek(file, 0, SEEK_SET);

    u8* rom_data = (u8*)malloc(rom_size);
    fread(rom_data, 1, rom_size, file);
    fclose(file);

    Result result = nes_init(nes, rom_data, rom_size, audio_sample_rate);
    free(rom_data);

    return result;
}

void nes_step_frame(NES* nes) {
    usize cpu_cycles = 0;
    
    while (true) {
        usize cpu_cycles = cpu_step(&nes->cpu);
        
        for (usize i = 0; i < cpu_cycles; i++) {
            apu_step(&nes->apu);
        }

        if (ppu_step(&nes->ppu, cpu_cycles * 3)) {
            break;
        }
    }
}

void nes_free(NES* nes) {
    cpu_free(&nes->cpu);
    ppu_free(&nes->ppu);
    cart_free(&nes->cart);
    nes->mapper->free(nes->mapper);
    free(nes->mapper);
}
