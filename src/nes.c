#include "nes.h"
#include "nrom.h"

Mapper *get_mapper(INES ines) {
    switch (ines.mapper_type) {
        case 0: {
            Mapper_NROM* nrom = (Mapper_NROM*)malloc(sizeof(Mapper_NROM));
            mapper_nrom_init((Mapper_NROM*)nrom);
            return (Mapper*)nrom;
        }
    }

    return NULL;
}

void nes_init(NES* nes, const char* rom_path) {
    // load ROM
    FILE* rom_file = fopen(rom_path, "rb");
    if (!rom_file) {
        printf("Failed to open ROM file: %s\n", rom_path);
        exit(1);
    }

    fseek(rom_file, 0, SEEK_END);
    long rom_size = ftell(rom_file);
    fseek(rom_file, 0, SEEK_SET);

    // read INES header
    u8 header[INES_HEADER_SIZE];
    fread(header, 1, INES_HEADER_SIZE, rom_file);
    INES ines = ines_parse(header);
    ines_print(ines);

    nes->mapper = get_mapper(ines);

    if (nes->mapper == NULL) {
        printf("Unsupported mapper: %d\n", ines.mapper_type);
        fclose(rom_file);
        exit(1);
    }

    printf("ines version: %d\n", ines.is_ines_2);

    if (ines.trainer) {
        // skip trainer data
        fseek(rom_file, 512, SEEK_SET);
    }

    // read PRG ROM
    usize prg_rom_size = ines.prg_banks * 16 * 1024;
    u8* prg_rom = (u8*)malloc(prg_rom_size);
    fread(prg_rom, 1, prg_rom_size, rom_file);

    // read CHR ROM
    usize chr_rom_size = ines.chr_banks * 8 * 1024;
    u8* chr_rom = (u8*)malloc(chr_rom_size);
    fread(chr_rom, 1, chr_rom_size, rom_file);
    fclose(rom_file);

    nes->cart = cart_create(ines, prg_rom, prg_rom_size, chr_rom, chr_rom_size);
    nes->mapper->init(nes->mapper, nes->cart);

    // initialize CPU, PPU and APU
    ppu_init(&nes->ppu, nes->cart);
    apu_init(&nes->apu, AUDIO_SAMPLE_RATE);
    cpu_init(&nes->cpu, nes->cart, &nes->ppu, &nes->apu, nes->mapper);
}

void nes_step_frame(NES* nes) {
    while (true) {
        usize cpu_cycles = cpu_step(&nes->cpu);
        
        if (ppu_step(&nes->ppu, cpu_cycles * 3)) {
            break;
        }
    }

    apu_step_frame(&nes->apu);
    ppu_render(&nes->ppu);
}

void nes_free(NES* nes) {
    cpu_free(&nes->cpu);
    ppu_free(&nes->ppu);
    cart_free(&nes->cart);
    nes->mapper->free(nes->mapper);
    free(nes->mapper);
}
