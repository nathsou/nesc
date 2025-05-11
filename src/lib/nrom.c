#include "nrom.h"

void nrom_init(Mapper* self, Cart* cart) {
    Mapper_NROM* nrom = (Mapper_NROM*)self;
    nrom->cart = cart;
    memset(nrom->prg_ram, 0, sizeof(nrom->prg_ram));
}

void nrom_free(Mapper* self) {}

void nrom_reset(Mapper* self) {}

void nrom_write(Mapper* self, u16 addr, u8 value) {
    if (addr >= 0x6000 && addr < 0x8000) {
        Mapper_NROM* nrom = (Mapper_NROM*)self;
        nrom->prg_ram[addr - 0x6000] = value;
    }
}

u8 nrom_read(Mapper* self, u16 addr) {
    Mapper_NROM* nrom = (Mapper_NROM*)self;

    if (addr < 0x2000) {
        return nrom->cart->chr_rom[addr];
    }

    if (addr >= 0x6000 && addr < 0x8000) {
        return nrom->prg_ram[addr - 0x6000];
    }

    if (addr >= 0x8000) {
        u16 prg_rom_addr = addr - 0x8000;

        if (nrom->cart->header.prg_banks == 1 && prg_rom_addr >= 0x4000) {
            prg_rom_addr -= 0x4000;
        }

        return nrom->cart->prg_rom[prg_rom_addr];
    }

    return 0;
}

void mapper_nrom_init(Mapper_NROM* mapper) {
    mapper->base.init = nrom_init;
    mapper->base.reset = nrom_reset;
    mapper->base.write = nrom_write;
    mapper->base.read = nrom_read;
    mapper->base.free = nrom_free;
}
