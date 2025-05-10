#ifndef NESC_MAPPER_NROM_H
#define NESC_MAPPER_NROM_H

#include "mapper.h"
#include <string.h>

typedef struct {
    Mapper base;
    Cart* cart;
    u8 prg_ram[2048];
} Mapper_NROM;

void mapper_nrom_init(Mapper_NROM* mapper);

#endif
