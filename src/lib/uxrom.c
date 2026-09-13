#include "uxrom.h"

#include <string.h>

static usize uxrom_prg_bank_count(const Mapper_UXROM* uxrom) {
    return uxrom->cart->prg_size / 0x4000;
}

static usize uxrom_prg_offset(const Mapper_UXROM* uxrom, usize bank, u16 addr) {
    usize bank_count = uxrom_prg_bank_count(uxrom);
    if (bank_count == 0) {
        return 0;
    }
    return (bank % bank_count) * 0x4000 + ((usize)addr & 0x3fff);
}

static void uxrom_reset(Mapper* self) {
    Mapper_UXROM* uxrom = (Mapper_UXROM*)self;
    uxrom->prg_bank = 0;
    memset(uxrom->prg_ram, 0, sizeof(uxrom->prg_ram));
    memset(uxrom->chr_ram, 0, sizeof(uxrom->chr_ram));
}

static void uxrom_init(Mapper* self, Cart* cart) {
    Mapper_UXROM* uxrom = (Mapper_UXROM*)self;
    uxrom->cart = cart;
    uxrom_reset(self);
}

static void uxrom_free(Mapper* self) {
    (void)self;
}

static void uxrom_write(Mapper* self, u16 addr, u8 value) {
    Mapper_UXROM* uxrom = (Mapper_UXROM*)self;

    if (addr < 0x2000) {
        if (uxrom->cart->chr_size == 0) {
            uxrom->chr_ram[addr] = value;
        }
    } else if (addr >= 0x6000 && addr < 0x8000) {
        uxrom->prg_ram[addr - 0x6000] = value;
    } else if (addr >= 0x8000) {
        uxrom->prg_bank = value;
    }
}

static u8 uxrom_read(Mapper* self, u16 addr) {
    Mapper_UXROM* uxrom = (Mapper_UXROM*)self;

    if (addr < 0x2000) {
        if (uxrom->cart->chr_size == 0) {
            return uxrom->chr_ram[addr];
        }
        return uxrom->cart->chr_rom[addr % uxrom->cart->chr_size];
    }

    if (addr >= 0x6000 && addr < 0x8000) {
        return uxrom->prg_ram[addr - 0x6000];
    }

    if (addr >= 0x8000 && uxrom->cart->prg_size != 0) {
        usize bank = addr < 0xc000 ? uxrom->prg_bank : uxrom_prg_bank_count(uxrom) - 1;
        return uxrom->cart->prg_rom[uxrom_prg_offset(uxrom, bank, addr)];
    }

    return 0;
}

void mapper_uxrom_init(Mapper_UXROM* mapper) {
    mapper->base.init = uxrom_init;
    mapper->base.reset = uxrom_reset;
    mapper->base.write = uxrom_write;
    mapper->base.read = uxrom_read;
    mapper->base.ppu_address = NULL;
    mapper->base.ppu_tick = NULL;
    mapper->base.is_asserting_irq = NULL;
    mapper->base.free = uxrom_free;
}
