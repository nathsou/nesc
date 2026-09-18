#include "nrom.h"

void nrom_init(Mapper* self, Cart* cart) {
    Mapper_NROM* nrom = (Mapper_NROM*)self;
    nrom->cart = cart;
    for (usize page = 0; page < 8; page++) {
        self->chr_pages[page] = cart->chr_size >= (page + 1) * 0x400
            ? cart->chr_rom + page * 0x400 : NULL;
    }
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
    memset(mapper->base.chr_pages, 0, sizeof(mapper->base.chr_pages));
    mapper->base.init = nrom_init;
    mapper->base.reset = nrom_reset;
    mapper->base.write = nrom_write;
    mapper->base.write_rmw = NULL;
    mapper->base.read = nrom_read;
    mapper->base.ppu_address = NULL;
    mapper->base.ppu_tick = NULL;
    mapper->base.ppu_advance = NULL;
    mapper->base.is_asserting_irq = NULL;
    mapper->base.free = nrom_free;
}
