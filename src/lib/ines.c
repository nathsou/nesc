#include "ines.h"
#include "nrom.h"

INES ines_parse(u8* header) {
    // Check for iNES header
    if (header[0] != 'N' || header[1] != 'E' || header[2] != 'S' || header[3] != 0x1A) {
        LOG("Invalid iNES header\n");
        exit(1);
    }

    INES ines;

    ines.prg_banks = header[4];
    ines.chr_banks = header[5];

    // Mapper type
    ines.mapper_type = ((header[6] & 0xF0) >> 4) | (header[7] & 0xF0);
    bool four_screen = (header[6] & 0b1000) != 0;
    bool mirroring_x = (header[6] & 1) != 0;
    ines.battery = (header[6] & 0x04) != 0;
    ines.trainer = (header[6] & 0x08) != 0;
    ines.is_ines_2 = (header[7] & 0x0C) == 0x08;

    if (four_screen) {
        ines.mirroring = NT_MIRRORING_FOUR_SCREEN;
    } else if (mirroring_x) {
        ines.mirroring = NT_MIRRORING_VERTICAL;
    } else {
        ines.mirroring = NT_MIRRORING_HORIZONTAL;
    }

    return ines;
}

char* mirroring_show(NametableMirroring mirroring) {
    switch (mirroring) {
        case NT_MIRRORING_HORIZONTAL:
            return "Horizontal";
        case NT_MIRRORING_VERTICAL:
            return "Vertical";
        case NT_MIRRORING_ONE_SCREEN_LOWER_BANK:
            return "One Screen Lower Bank";
        case NT_MIRRORING_ONE_SCREEN_UPPER_BANK:
            return "One Screen Upper Bank";
        case NT_MIRRORING_FOUR_SCREEN:
            return "Four Screen";
        default:
            return "Unknown";
    }
}

void ines_print(INES ines) {
    LOG("PRG banks: %d\n", ines.prg_banks);
    LOG("CHR banks: %d\n", ines.chr_banks);
    LOG("Mapper type: %d\n", ines.mapper_type);
    LOG("Mirroring: %s\n", mirroring_show(ines.mirroring));
    LOG("Battery: %s\n", ines.battery ? "Yes" : "No");
    LOG("Trainer: %s\n", ines.trainer ? "Yes" : "No");
}
