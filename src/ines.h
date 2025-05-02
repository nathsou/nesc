#ifndef __NESC_MAPPER_H__
#define __NESC_MAPPER_H__

#include "types.h"
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    u8 prg_banks;
    u8 chr_banks;
    u8 mapper_type;
    bool mirroring_x;
    bool mirroring_y;
    bool battery;
    bool trainer;
} INES;

INES ines_parse(u8* header);
void ines_print(INES ines);

#endif
