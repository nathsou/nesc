#include "mmc3.h"

#include <string.h>

// MMC3 samples PPU A12 through a three-M2-edge filter. At the NTSC PPU
// clock this means A12 must remain low for at least eight PPU cycles before
// a subsequent rising edge clocks the IRQ counter.
#define MMC3_A12_LOW_FILTER_CYCLES 8

static usize mmc3_prg_bank_count(const Mapper_MMC3* mmc3) {
    return mmc3->cart->prg_size / 0x2000;
}

static usize mmc3_chr_bank_count(const Mapper_MMC3* mmc3) {
    usize size = mmc3->cart->chr_size == 0 ? sizeof(mmc3->chr_ram) : mmc3->cart->chr_size;
    return size / 0x400;
}

static usize mmc3_prg_offset(const Mapper_MMC3* mmc3, usize bank, u16 addr) {
    usize count = mmc3_prg_bank_count(mmc3);
    if (count == 0) {
        return 0;
    }
    return (bank % count) * 0x2000 + ((usize)addr & 0x1fff);
}

static usize mmc3_chr_offset(const Mapper_MMC3* mmc3, usize bank, u16 addr) {
    usize count = mmc3_chr_bank_count(mmc3);
    if (count == 0) {
        return 0;
    }
    return (bank % count) * 0x400 + ((usize)addr & 0x3ff);
}

static usize mmc3_prg_bank_for_addr(const Mapper_MMC3* mmc3, u16 addr) {
    usize count = mmc3_prg_bank_count(mmc3);
    usize second_last = count >= 2 ? count - 2 : 0;
    usize last = count != 0 ? count - 1 : 0;

    switch ((addr - 0x8000) >> 13) {
        case 0: return mmc3->prg_mode ? second_last : mmc3->bank_registers[6];
        case 1: return mmc3->bank_registers[7];
        case 2: return mmc3->prg_mode ? mmc3->bank_registers[6] : second_last;
        default: return last;
    }
}

static usize mmc3_chr_bank_for_addr(const Mapper_MMC3* mmc3, u16 addr) {
    usize page = addr >> 10;
    usize bank;

    if (!mmc3->chr_inversion) {
        switch (page) {
            case 0: case 1: bank = (mmc3->bank_registers[0] & 0xfe) + (page & 1); break;
            case 2: case 3: bank = (mmc3->bank_registers[1] & 0xfe) + (page & 1); break;
            default: bank = mmc3->bank_registers[page - 2]; break;
        }
    } else {
        switch (page) {
            case 0: case 1: case 2: case 3: bank = mmc3->bank_registers[page + 2]; break;
            case 4: case 5: bank = (mmc3->bank_registers[0] & 0xfe) + (page & 1); break;
            default: bank = (mmc3->bank_registers[1] & 0xfe) + (page & 1); break;
        }
    }

    return bank;
}

static void mmc3_reset(Mapper* self) {
    Mapper_MMC3* mmc3 = (Mapper_MMC3*)self;
    memset(mmc3->bank_registers, 0, sizeof(mmc3->bank_registers));
    memset(mmc3->prg_ram, 0, sizeof(mmc3->prg_ram));
    memset(mmc3->chr_ram, 0, sizeof(mmc3->chr_ram));
    mmc3->bank_select = 0;
    mmc3->prg_mode = false;
    mmc3->chr_inversion = false;
    mmc3->prg_ram_enabled = true;
    mmc3->prg_ram_write_protected = false;
    mmc3->irq_latch = 0;
    mmc3->irq_counter = 0;
    mmc3->irq_reload = false;
    mmc3->irq_enabled = false;
    mmc3->irq_pending = false;
    mmc3->a12_high = false;
    mmc3->a12_low_cycles = 0;
}

static void mmc3_init(Mapper* self, Cart* cart) {
    Mapper_MMC3* mmc3 = (Mapper_MMC3*)self;
    mmc3->cart = cart;
    mmc3_reset(self);
}

static void mmc3_free(Mapper* self) {
    (void)self;
}

static void mmc3_clock_irq(Mapper_MMC3* mmc3) {
    if (mmc3->irq_counter == 0 || mmc3->irq_reload) {
        mmc3->irq_counter = mmc3->irq_latch;
        mmc3->irq_reload = false;
    } else {
        mmc3->irq_counter--;
    }

    if (mmc3->irq_counter == 0 && mmc3->irq_enabled) {
        mmc3->irq_pending = true;
    }
}

static void mmc3_ppu_address(Mapper* self, u16 addr, usize ppu_cycle) {
    Mapper_MMC3* mmc3 = (Mapper_MMC3*)self;
    bool a12 = (addr & 0x1000) != 0;
    (void)ppu_cycle;

    if (!a12) {
        if (mmc3->a12_high) {
            mmc3->a12_low_cycles = 0;
        }
        mmc3->a12_high = false;
    } else {
        if (!mmc3->a12_high && mmc3->a12_low_cycles >= MMC3_A12_LOW_FILTER_CYCLES) {
            mmc3_clock_irq(mmc3);
        }
        mmc3->a12_high = true;
    }
}

static void mmc3_ppu_tick(Mapper* self) {
    Mapper_MMC3* mmc3 = (Mapper_MMC3*)self;
    if (!mmc3->a12_high && mmc3->a12_low_cycles < MMC3_A12_LOW_FILTER_CYCLES) {
        mmc3->a12_low_cycles++;
    }
}

static bool mmc3_is_asserting_irq(Mapper* self) {
    return ((Mapper_MMC3*)self)->irq_pending;
}

static void mmc3_write(Mapper* self, u16 addr, u8 value) {
    Mapper_MMC3* mmc3 = (Mapper_MMC3*)self;

    if (addr < 0x2000) {
        if (mmc3->cart->chr_size == 0) {
            mmc3->chr_ram[mmc3_chr_offset(mmc3, mmc3_chr_bank_for_addr(mmc3, addr), addr)] = value;
        }
        return;
    }

    if (addr >= 0x6000 && addr < 0x8000) {
        if (mmc3->prg_ram_enabled && !mmc3->prg_ram_write_protected) {
            mmc3->prg_ram[addr - 0x6000] = value;
        }
        return;
    }

    if (addr < 0x8000) {
        return;
    }

    switch (addr & 0xe001) {
        case 0x8000:
            mmc3->bank_select = value & 7;
            mmc3->prg_mode = (value & 0x40) != 0;
            mmc3->chr_inversion = (value & 0x80) != 0;
            break;
        case 0x8001:
            mmc3->bank_registers[mmc3->bank_select] = value;
            break;
        case 0xa000:
            if (mmc3->cart->header.mirroring != NT_MIRRORING_FOUR_SCREEN) {
                // Bit 0 selects CIRAM A10: 0 = PPU A10 (vertical
                // mirroring), 1 = PPU A11 (horizontal mirroring).
                mmc3->cart->header.mirroring = value & 1 ? NT_MIRRORING_HORIZONTAL : NT_MIRRORING_VERTICAL;
            }
            break;
        case 0xa001:
            mmc3->prg_ram_enabled = (value & 0x80) != 0;
            mmc3->prg_ram_write_protected = (value & 0x40) != 0;
            break;
        case 0xc000:
            mmc3->irq_latch = value;
            break;
        case 0xc001:
            mmc3->irq_counter = 0;
            mmc3->irq_reload = true;
            break;
        case 0xe000:
            mmc3->irq_enabled = false;
            mmc3->irq_pending = false;
            break;
        case 0xe001:
            mmc3->irq_enabled = true;
            break;
        default:
            break;
    }
}

static u8 mmc3_read(Mapper* self, u16 addr) {
    Mapper_MMC3* mmc3 = (Mapper_MMC3*)self;

    if (addr < 0x2000) {
        usize offset = mmc3_chr_offset(mmc3, mmc3_chr_bank_for_addr(mmc3, addr), addr);
        return mmc3->cart->chr_size == 0 ? mmc3->chr_ram[offset] : mmc3->cart->chr_rom[offset];
    }

    if (addr >= 0x6000 && addr < 0x8000) {
        return mmc3->prg_ram_enabled ? mmc3->prg_ram[addr - 0x6000] : 0;
    }

    if (addr >= 0x8000 && mmc3->cart->prg_size != 0) {
        return mmc3->cart->prg_rom[mmc3_prg_offset(mmc3, mmc3_prg_bank_for_addr(mmc3, addr), addr)];
    }

    return 0;
}

void mapper_mmc3_init(Mapper_MMC3* mapper) {
    mapper->base.init = mmc3_init;
    mapper->base.reset = mmc3_reset;
    mapper->base.write = mmc3_write;
    mapper->base.write_rmw = NULL;
    mapper->base.read = mmc3_read;
    mapper->base.ppu_address = mmc3_ppu_address;
    mapper->base.ppu_tick = mmc3_ppu_tick;
    mapper->base.is_asserting_irq = mmc3_is_asserting_irq;
    mapper->base.free = mmc3_free;
}
