#ifdef NESC_COMPONENT_PROFILE
#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 200809L
#endif
#endif

#include "nes.h"
#include "nrom.h"
#include "mmc1.h"
#include "uxrom.h"
#include "mmc3.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#ifdef NESC_COMPONENT_PROFILE
#include <signal.h>
#include <sys/time.h>

static NESComponentProfile nes_component_profile;
static volatile sig_atomic_t nes_profile_phase = NES_PROFILE_OTHER;
static volatile sig_atomic_t nes_profile_phase_samples[NES_PROFILE_PHASE_COUNT];
static struct sigaction nes_profile_previous_action;
static bool nes_profile_action_installed;

static void nes_profile_sample_handler(int signal_number) {
    (void)signal_number;
    sig_atomic_t phase = nes_profile_phase;
    if (phase >= 0 && phase < NES_PROFILE_PHASE_COUNT) {
        nes_profile_phase_samples[phase]++;
    }
}

void nes_component_profile_reset(void) {
    memset(&nes_component_profile, 0, sizeof(nes_component_profile));
    for (usize i = 0; i < NES_PROFILE_PHASE_COUNT; i++) {
        nes_profile_phase_samples[i] = 0;
    }
    nes_profile_phase = NES_PROFILE_OTHER;
}

bool nes_component_profile_start(void) {
    struct sigaction action = {0};
    action.sa_handler = nes_profile_sample_handler;
    sigemptyset(&action.sa_mask);
    action.sa_flags = 0;
    if (sigaction(SIGPROF, &action, &nes_profile_previous_action) != 0) {
        return false;
    }
    nes_profile_action_installed = true;

    struct itimerval timer = {0};
    timer.it_value.tv_usec = 1000;
    timer.it_interval.tv_usec = 1000;
    if (setitimer(ITIMER_PROF, &timer, NULL) != 0) {
        nes_component_profile_stop();
        return false;
    }
    return true;
}

void nes_component_profile_stop(void) {
    // The headless profiler is single-threaded. Drain a pending timer signal
    // before restoring its previous handler (which may terminate the process).
    sigset_t blocked, previous_mask, pending;
    sigemptyset(&blocked);
    sigaddset(&blocked, SIGPROF);
    sigprocmask(SIG_BLOCK, &blocked, &previous_mask);
    struct itimerval timer = {0};
    setitimer(ITIMER_PROF, &timer, NULL);
    if (sigpending(&pending) == 0 && sigismember(&pending, SIGPROF)) {
        int signal_number;
        sigwait(&blocked, &signal_number);
    }
    nes_profile_phase = NES_PROFILE_OTHER;
    if (nes_profile_action_installed) {
        sigaction(SIGPROF, &nes_profile_previous_action, NULL);
        nes_profile_action_installed = false;
    }
    sigprocmask(SIG_SETMASK, &previous_mask, NULL);
}

NESComponentProfile nes_component_profile_read(void) {
    for (usize i = 0; i < NES_PROFILE_PHASE_COUNT; i++) {
        nes_component_profile.phase_samples[i] = (u64)nes_profile_phase_samples[i];
    }
    return nes_component_profile;
}
#endif

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
        case 2: {
            Mapper_UXROM* uxrom = (Mapper_UXROM*)malloc(sizeof(Mapper_UXROM));
            mapper_uxrom_init(uxrom);
            return (Mapper*)uxrom;
        }
        case 4: {
            Mapper_MMC3* mmc3 = (Mapper_MMC3*)malloc(sizeof(Mapper_MMC3));
            mapper_mmc3_init(mmc3);
            return (Mapper*)mmc3;
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
#ifdef NESC_COMPONENT_PROFILE
        nes_profile_phase = NES_PROFILE_CPU;
#endif
        usize cpu_cycles = cpu_step(&nes->cpu);

#ifdef NESC_COMPONENT_PROFILE
        nes_profile_phase = NES_PROFILE_APU;
#endif
        
        for (usize i = 0; i < cpu_cycles; i++) {
            apu_step(&nes->apu);
        }

#ifdef NESC_COMPONENT_PROFILE
        nes_profile_phase = NES_PROFILE_PPU;
#endif

        bool frame_complete = ppu_step(&nes->ppu, cpu_cycles * 3);

#ifdef NESC_COMPONENT_PROFILE
        nes_profile_phase = NES_PROFILE_OTHER;
#endif

        if (frame_complete) {
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
