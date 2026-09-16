#include "cart.h"
#include "mmc3.h"
#include "mmc1.h"
#include "uxrom.h"
#include "ppu.h"

#include <stdio.h>
#include <string.h>

static int failures = 0;

#define EXPECT_EQ(actual, expected) do { \
    unsigned got_value = (unsigned)(actual); \
    unsigned expected_value = (unsigned)(expected); \
    if (got_value != expected_value) { \
        fprintf(stderr, "%s:%d: expected %u, got %u\n", __FILE__, __LINE__, expected_value, got_value); \
        failures++; \
    } \
} while (0)

static INES test_header(usize prg_banks, usize chr_banks, NametableMirroring mirroring) {
    INES header = {0};
    header.prg_banks = (u8)prg_banks;
    header.chr_banks = (u8)chr_banks;
    header.mirroring = mirroring;
    return header;
}

static void test_uxrom(void) {
    u8 prg[4 * 0x4000];
    u8 chr_ram_backing[1] = {0};
    for (usize bank = 0; bank < 4; bank++) {
        memset(prg + bank * 0x4000, (int)(0x10 + bank), 0x4000);
    }

    Cart cart = cart_create(test_header(4, 0, NT_MIRRORING_HORIZONTAL), prg, sizeof(prg), chr_ram_backing, 0);
    Mapper_UXROM uxrom = {0};
    mapper_uxrom_init(&uxrom);
    uxrom.base.init((Mapper*)&uxrom, &cart);

    EXPECT_EQ(uxrom.base.read((Mapper*)&uxrom, 0x8000), 0x10);
    EXPECT_EQ(uxrom.base.read((Mapper*)&uxrom, 0xc000), 0x13);
    uxrom.base.write((Mapper*)&uxrom, 0x8000, 2);
    EXPECT_EQ(uxrom.base.read((Mapper*)&uxrom, 0x8000), 0x12);
    EXPECT_EQ(uxrom.base.read((Mapper*)&uxrom, 0xc000), 0x13);
    uxrom.base.write((Mapper*)&uxrom, 0x6000, 0xa5);
    EXPECT_EQ(uxrom.base.read((Mapper*)&uxrom, 0x6000), 0xa5);
    uxrom.base.write((Mapper*)&uxrom, 0x0010, 0x5a);
    EXPECT_EQ(uxrom.base.read((Mapper*)&uxrom, 0x0010), 0x5a);
}

static void test_mmc1_rmw_write_filter(void) {
    u8 prg[2 * 0x4000] = {0};
    u8 chr[0x2000] = {0};
    Cart cart = cart_create(test_header(2, 1, NT_MIRRORING_HORIZONTAL), prg, sizeof(prg), chr, sizeof(chr));
    Mapper_MMC1 mmc1 = {0};
    mapper_mmc1_init(&mmc1);
    mmc1.base.init((Mapper*)&mmc1, &cart);
    mmc1.shift_reg = 0b10000;

    mmc1.base.write_rmw((Mapper*)&mmc1, 0xe000, 1, 0);
    EXPECT_EQ(mmc1.shift_reg, 0b11000);
}

static void mmc3_write_register(Mapper_MMC3* mmc3, u8 reg, u8 value, u8 modes) {
    mmc3->base.write((Mapper*)mmc3, 0x8000, (u8)(modes | reg));
    mmc3->base.write((Mapper*)mmc3, 0x8001, value);
}

static void mmc3_tick_ppu(Mapper_MMC3* mmc3, usize cycles) {
    for (usize i = 0; i < cycles; i++) {
        mapper_ppu_tick((Mapper*)mmc3);
    }
}

static void test_mmc3(void) {
    u8 prg[8 * 0x2000];
    u8 chr[16 * 0x400];
    for (usize bank = 0; bank < 8; bank++) {
        memset(prg + bank * 0x2000, (int)(0x20 + bank), 0x2000);
    }
    for (usize bank = 0; bank < 16; bank++) {
        memset(chr + bank * 0x400, (int)(0x40 + bank), 0x400);
    }

    Cart cart = cart_create(test_header(4, 2, NT_MIRRORING_HORIZONTAL), prg, sizeof(prg), chr, sizeof(chr));
    Mapper_MMC3 mmc3 = {0};
    mapper_mmc3_init(&mmc3);
    mmc3.base.init((Mapper*)&mmc3, &cart);

    mmc3_write_register(&mmc3, 6, 3, 0);
    mmc3_write_register(&mmc3, 7, 4, 0);
    EXPECT_EQ(mmc3.base.read((Mapper*)&mmc3, 0x8000), 0x23);
    EXPECT_EQ(mmc3.base.read((Mapper*)&mmc3, 0xa000), 0x24);
    EXPECT_EQ(mmc3.base.read((Mapper*)&mmc3, 0xc000), 0x26);
    EXPECT_EQ(mmc3.base.read((Mapper*)&mmc3, 0xe000), 0x27);

    mmc3_write_register(&mmc3, 6, 3, 0x40);
    EXPECT_EQ(mmc3.base.read((Mapper*)&mmc3, 0x8000), 0x26);
    EXPECT_EQ(mmc3.base.read((Mapper*)&mmc3, 0xc000), 0x23);

    mmc3_write_register(&mmc3, 0, 2, 0);
    mmc3_write_register(&mmc3, 1, 4, 0);
    mmc3_write_register(&mmc3, 2, 6, 0);
    mmc3_write_register(&mmc3, 3, 7, 0);
    mmc3_write_register(&mmc3, 4, 8, 0);
    mmc3_write_register(&mmc3, 5, 9, 0);
    for (usize page = 0; page < 8; page++) {
        EXPECT_EQ(mmc3.base.read((Mapper*)&mmc3, (u16)(page * 0x400)), 0x40 + (u8)(page + 2));
    }
    mmc3.base.write((Mapper*)&mmc3, 0x8000, 0x80);
    const u8 inverted_banks[] = {6, 7, 8, 9, 2, 3, 4, 5};
    for (usize page = 0; page < 8; page++) {
        EXPECT_EQ(mmc3.base.read((Mapper*)&mmc3, (u16)(page * 0x400)), 0x40 + inverted_banks[page]);
    }

    mmc3.base.write((Mapper*)&mmc3, 0xa000, 1);
    EXPECT_EQ(cart.header.mirroring, NT_MIRRORING_HORIZONTAL);
    cart.header.mirroring = NT_MIRRORING_FOUR_SCREEN;
    mmc3.base.write((Mapper*)&mmc3, 0xa000, 0);
    EXPECT_EQ(cart.header.mirroring, NT_MIRRORING_FOUR_SCREEN);
    mmc3.base.write((Mapper*)&mmc3, 0xa001, 0x80);
    mmc3.base.write((Mapper*)&mmc3, 0x6000, 0x5c);
    EXPECT_EQ(mmc3.base.read((Mapper*)&mmc3, 0x6000), 0x5c);
    mmc3.base.write((Mapper*)&mmc3, 0xa001, 0xc0);
    mmc3.base.write((Mapper*)&mmc3, 0x6000, 0x00);
    EXPECT_EQ(mmc3.base.read((Mapper*)&mmc3, 0x6000), 0x5c);

    mmc3.base.write((Mapper*)&mmc3, 0xc000, 1);
    mmc3.base.write((Mapper*)&mmc3, 0xc001, 0);
    mmc3.base.write((Mapper*)&mmc3, 0xe001, 0);
    mapper_ppu_address((Mapper*)&mmc3, 0x0000, 0);
    mmc3_tick_ppu(&mmc3, 8);
    mapper_ppu_address((Mapper*)&mmc3, 0x1000, 3);
    EXPECT_EQ(mapper_is_asserting_irq((Mapper*)&mmc3), false);
    mapper_ppu_address((Mapper*)&mmc3, 0x0000, 4);
    mmc3_tick_ppu(&mmc3, 8);
    mapper_ppu_address((Mapper*)&mmc3, 0x1000, 7);
    EXPECT_EQ(mapper_is_asserting_irq((Mapper*)&mmc3), true);
    mmc3.base.write((Mapper*)&mmc3, 0xe000, 0);
    EXPECT_EQ(mapper_is_asserting_irq((Mapper*)&mmc3), false);

    mmc3.base.write((Mapper*)&mmc3, 0xc000, 0);
    mmc3.base.write((Mapper*)&mmc3, 0xc001, 0);
    mmc3.base.write((Mapper*)&mmc3, 0xe001, 0);
    mapper_ppu_address((Mapper*)&mmc3, 0x0000, 8);
    mmc3_tick_ppu(&mmc3, 7);
    mapper_ppu_address((Mapper*)&mmc3, 0x1000, 10);
    EXPECT_EQ(mapper_is_asserting_irq((Mapper*)&mmc3), false);
    mapper_ppu_address((Mapper*)&mmc3, 0x0000, 12);
    mmc3_tick_ppu(&mmc3, 8);
    mapper_ppu_address((Mapper*)&mmc3, 0x1000, 15);
    EXPECT_EQ(mapper_is_asserting_irq((Mapper*)&mmc3), true);
}

static void test_mmc3_nametable_mapping(void) {
    Cart cart = {0};
    Mapper_MMC3 mmc3 = {0};
    PPU ppu = {0};
    mapper_mmc3_init(&mmc3);
    mmc3.base.init((Mapper*)&mmc3, &cart);
    ppu_init(&ppu, &cart, (Mapper*)&mmc3);

    // Test actual RAM aliases, including attribute bytes. Vertical mirroring
    // must preserve two independent nametables for horizontal scrolling.
    const u16 offsets[] = {0, 31, 0x3bf, 0x3c0, 0x3ff};
    for (usize i = 0; i < sizeof(offsets) / sizeof(offsets[0]); i++) {
        u16 offset = offsets[i];
        mmc3.base.write((Mapper*)&mmc3, 0xa000, 0);
        ppu_write(&ppu, (u16)(0x2000 + offset), 0x11);
        ppu_write(&ppu, (u16)(0x2400 + offset), 0x22);
        EXPECT_EQ(ppu_read(&ppu, (u16)(0x2000 + offset)), 0x11);
        EXPECT_EQ(ppu_read(&ppu, (u16)(0x2400 + offset)), 0x22);
        EXPECT_EQ(ppu_read(&ppu, (u16)(0x2800 + offset)), 0x11);
        EXPECT_EQ(ppu_read(&ppu, (u16)(0x2c00 + offset)), 0x22);

        // Odd data bits above bit 0 are ignored; even register aliases work.
        mmc3.base.write((Mapper*)&mmc3, 0xbffe, 0xff);
        ppu_write(&ppu, (u16)(0x2000 + offset), 0x33);
        ppu_write(&ppu, (u16)(0x2800 + offset), 0x44);
        EXPECT_EQ(ppu_read(&ppu, (u16)(0x2000 + offset)), 0x33);
        EXPECT_EQ(ppu_read(&ppu, (u16)(0x2400 + offset)), 0x33);
        EXPECT_EQ(ppu_read(&ppu, (u16)(0x2800 + offset)), 0x44);
        EXPECT_EQ(ppu_read(&ppu, (u16)(0x2c00 + offset)), 0x44);
    }
}

int main(void) {
    test_mmc1_rmw_write_filter();
    test_uxrom();
    test_mmc3();
    test_mmc3_nametable_mapping();
    if (failures != 0) {
        fprintf(stderr, "%d mapper test(s) failed\n", failures);
        return 1;
    }
    puts("mapper tests passed");
    return 0;
}
