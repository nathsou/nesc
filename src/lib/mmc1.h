#ifndef NESC_MMC1_H
#define NESC_MMC1_H

#include "mapper.h"
#include <string.h>

typedef struct {
    Mapper base;
    Cart* cart;
    u8 prg_ram[0x2000];
    u8 chr_ram[0x2000];
    u8 shift_reg;
    u8 control_reg;
    u8 prg_mode;
    u8 chr_mode;
    u8 chr_bank[2];
    u8 prg_bank;
} Mapper_MMC1;

void mapper_mmc1_init(Mapper_MMC1* mapper);

#endif
