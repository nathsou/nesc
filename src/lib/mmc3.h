#ifndef NESC_MMC3_H
#define NESC_MMC3_H

#include "mapper.h"

typedef struct {
    Mapper base;
    Cart* cart;
    u8 prg_ram[0x2000];
    u8 chr_ram[0x2000];
    u8 bank_registers[8];
    u8 bank_select;
    bool prg_mode;
    bool chr_inversion;
    bool prg_ram_enabled;
    bool prg_ram_write_protected;
    u8 irq_latch;
    u8 irq_counter;
    bool irq_reload;
    bool irq_enabled;
    bool irq_pending;
    bool a12_high;
    u8 a12_low_cycles;
} Mapper_MMC3;

void mapper_mmc3_init(Mapper_MMC3* mapper);

#endif
