#include "mmc1.h"

void mmc1_reset(Mapper* self);

void mmc1_init(Mapper* self, Cart* cart) {
    Mapper_MMC1* mmc1 = (Mapper_MMC1*)self;
    mmc1->cart = cart;
    mmc1_reset(self);
}

void mmc1_free(Mapper* self) {}

void mmc1_reset(Mapper* self) {
    Mapper_MMC1* mmc1 = (Mapper_MMC1*)self;
    mmc1->shift_reg = 0;
    mmc1->control_reg = 0;
    mmc1->prg_mode = 3; // https://forums.nesdev.org/viewtopic.php?t=6766
    mmc1->chr_mode = 0;
    mmc1->prg_bank = 0;
    memset(mmc1->prg_ram, 0, sizeof(mmc1->prg_ram));
    memset(mmc1->chr_ram, 0, sizeof(mmc1->chr_ram));
}

void mmc1_write_control(Mapper_MMC1* mmc1, u8 value) {
    // CPPMM
    mmc1->control_reg = value;
    mmc1->prg_mode = (value >> 2) & 3;
    mmc1->chr_mode = (value >> 4) & 1;

    NametableMirroring mirroring;

    switch (value & 3) {
        case 0: mirroring = NT_MIRRORING_ONE_SCREEN_LOWER_BANK; break;
        case 1: mirroring = NT_MIRRORING_ONE_SCREEN_UPPER_BANK; break;
        case 2: mirroring = NT_MIRRORING_VERTICAL; break;
        case 3: mirroring = NT_MIRRORING_HORIZONTAL; break;
    }

    mmc1->cart->header.mirroring = mirroring;
}

usize mmc1_chr_rom_offset(Mapper_MMC1* mmc1, u16 addr) {
    if (mmc1->chr_mode == 0) {
        // switch 8 KB at a time
        if (addr < 4096) {
            return (addr & 0xFFF) + (mmc1->chr_bank[0] * 4096);
        } else {
            return (addr & 0xFFF) + ((mmc1->chr_bank[0] | 1) * 4096);
        }
    } else {
        // switch two separate 4 KB banks
        if (addr < 4096) {
            return (addr & 0xFFF) + (mmc1->chr_bank[0] * 4096);
        } else {
            return (addr & 0xFFF) + (mmc1->chr_bank[1] * 4096);
        }
    }
}

void mmc1_write(Mapper* self, u16 addr, u8 value) {
    Mapper_MMC1* mmc1 = (Mapper_MMC1*)self;

    if (addr < 0x2000) {
        if (mmc1->cart->chr_size == 0) {
            mmc1->chr_ram[addr] = value;
        } else {
            usize offset = mmc1_chr_rom_offset(mmc1, addr);
            mmc1->cart->chr_rom[offset] = value;
        }
    } else if (addr >= 0x6000 && addr < 0x8000) {
        mmc1->prg_ram[addr - 0x6000] = value;
    } else if (addr >= 0x8000) {
        if (value & (1 << 7)) {
            // reset the shift register
            mmc1->shift_reg = 0b10000;
            mmc1_write_control(mmc1, mmc1->control_reg | 0x0C);
        } else {
            bool done = mmc1->shift_reg & 1;
            mmc1->shift_reg = ((mmc1->shift_reg >> 1) | ((value & 1) << 4)) & 0b11111;

            if (done) {
                if (addr >= 0x8000 && addr <= 0x9FFF) {
                    mmc1_write_control(mmc1, mmc1->shift_reg);
                } else if (addr <= 0xBFFF) {
                    mmc1->chr_bank[0] = mmc1->shift_reg & 0b11111;
                } else if (addr <= 0xDFFF) {
                    mmc1->chr_bank[1] = mmc1->shift_reg & 0b11111;
                } else {
                    mmc1->prg_bank = mmc1->shift_reg & 0b1111;
                }

                mmc1->shift_reg = 0b10000;
            }
        }
    }
}

u8 mmc1_read(Mapper* self, u16 addr) {
    Mapper_MMC1* mmc1 = (Mapper_MMC1*)self;

    if (addr < 0x2000) {
        if (mmc1->cart->chr_size == 0) {
            return mmc1->chr_ram[addr];
        } else {
            usize offset = mmc1_chr_rom_offset(mmc1, addr);
            return mmc1->cart->chr_rom[offset];
        }
    }
    
    if (addr < 0x8000) {
        return mmc1->prg_ram[addr - 0x6000];
    }
    
    if (addr < 0xC000) {
        usize bank = 0;

        switch (mmc1->prg_mode) {
            case 0:
            case 1:
                bank = mmc1->prg_bank & 0xFE;
                break;
            case 2:
                bank = 0;
                break;
            case 3:
                bank = mmc1->prg_bank;
                break;
        }

        return mmc1->cart->prg_rom[(addr - 0x8000) + (bank * 16384)];
    }
    
    {
        usize bank = 0;

        switch (mmc1->prg_mode) {
            case 0:
            case 1:
                bank = mmc1->prg_bank | 1;
                break;
            case 2:
                bank = mmc1->prg_bank;
                break;
            case 3:
                bank = mmc1->cart->header.prg_banks - 1;
                break;
        }

        return mmc1->cart->prg_rom[((addr - 0x8000) & 0x3fff) + (bank * 16384)];
    }
}

void mapper_mmc1_init(Mapper_MMC1 *mapper) {
    mapper->base.init = mmc1_init;
    mapper->base.reset = mmc1_reset;
    mapper->base.write = mmc1_write;
    mapper->base.read = mmc1_read;
    mapper->base.free = mmc1_free;
}
