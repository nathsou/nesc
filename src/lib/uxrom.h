#ifndef NESC_UXROM_H
#define NESC_UXROM_H

#include "mapper.h"

typedef struct {
    Mapper base;
    Cart* cart;
    u8 prg_ram[0x2000];
    u8 chr_ram[0x2000];
    u8 prg_bank;
} Mapper_UXROM;

void mapper_uxrom_init(Mapper_UXROM* mapper);

#endif
