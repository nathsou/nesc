#include "ines.h"

INES ines_parse(u8* header) {
    // Check for iNES header
    if (header[0] != 'N' || header[1] != 'E' || header[2] != 'S' || header[3] != 0x1A) {
        printf("Invalid iNES header\n");
        exit(1);
    }

    INES ines;

    ines.prg_banks = header[4];
    ines.chr_banks = header[5];

    // Mapper type
    ines.mapper_type = ((header[6] & 0xF0) >> 4) | (header[7] & 0xF0);
    ines.mirroring_x = (header[6] & 0x01) != 0;
    ines.mirroring_y = (header[6] & 0x02) != 0;
    ines.battery = (header[6] & 0x04) != 0;
    ines.trainer = (header[6] & 0x08) != 0;
    ines.is_ines_2 = (header[7] & 0x0C) == 0x08;

    return ines;
}

void ines_print(INES ines) {
    printf("PRG banks: %d\n", ines.prg_banks);
    printf("CHR banks: %d\n", ines.chr_banks);
    printf("Mapper type: %d\n", ines.mapper_type);
    printf("Mirroring: %s\n", ines.mirroring_x ? "Horizontal" : "Vertical");
    printf("Battery: %s\n", ines.battery ? "Yes" : "No");
    printf("Trainer: %s\n", ines.trainer ? "Yes" : "No");
}

bool ines_is_supported(INES ines) {
    return ines.mapper_type == 0;
}
