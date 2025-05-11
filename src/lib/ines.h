#ifndef __NESC_MAPPER_H__
#define __NESC_MAPPER_H__

#include "types.h"
#include <stdio.h>
#include <stdlib.h>

typedef enum {
    NT_MIRRORING_HORIZONTAL,
    NT_MIRRORING_VERTICAL,
    NT_MIRRORING_ONE_SCREEN_LOWER_BANK,
    NT_MIRRORING_ONE_SCREEN_UPPER_BANK,
    NT_MIRRORING_FOUR_SCREEN
} NametableMirroring;

typedef struct {
    u8 prg_banks;
    u8 chr_banks;
    u8 mapper_type;
    NametableMirroring mirroring;
    bool battery;
    bool trainer;
    bool is_ines_2;
} INES;

INES ines_parse(u8* header);
void ines_print(INES ines);

#endif