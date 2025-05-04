#include "ppu.h"
#include "cpu.h"

#define SPRITES_PALETTES_OFFSET 0x11
#define BYTES_PER_PALETTE 4
#define TILES_PER_ROW 32
#define TILES_PER_COLUMN 30

u8* chr_rom;
u8 nametable[2048];
u8 palette_table[32];
u8 oam[256];

u16 ppu_v;
u8 ppu_w;
u8 ppu_f;

u8 ppu_ctrl;
u8 ppu_mask;
u8 ppu_status;
u8 oam_addr;
u8 ppu_scroll_x;
u8 ppu_scroll_y;

u16 vram_addr;
u8 vram_internal_buffer;
u8 oam_dma;

u8 frame[SCREEN_WIDTH * SCREEN_HEIGHT * 3];
bool opaque_bg_mask[SCREEN_WIDTH * SCREEN_HEIGHT];

usize scanlines;
usize dots;
usize frame_count;
CartMetadata cart;

// 64 RGB colors
const u8 COLOR_PALETTE[] = {
   0x80, 0x80, 0x80, 0x00, 0x3D, 0xA6, 0x00, 0x12, 0xB0, 0x44, 0x00, 0x96, 0xA1, 0x00, 0x5E,
   0xC7, 0x00, 0x28, 0xBA, 0x06, 0x00, 0x8C, 0x17, 0x00, 0x5C, 0x2F, 0x00, 0x10, 0x45, 0x00,
   0x05, 0x4A, 0x00, 0x00, 0x47, 0x2E, 0x00, 0x41, 0x66, 0x00, 0x00, 0x00, 0x05, 0x05, 0x05,
   0x05, 0x05, 0x05, 0xC7, 0xC7, 0xC7, 0x00, 0x77, 0xFF, 0x21, 0x55, 0xFF, 0x82, 0x37, 0xFA,
   0xEB, 0x2F, 0xB5, 0xFF, 0x29, 0x50, 0xFF, 0x22, 0x00, 0xD6, 0x32, 0x00, 0xC4, 0x62, 0x00,
   0x35, 0x80, 0x00, 0x05, 0x8F, 0x00, 0x00, 0x8A, 0x55, 0x00, 0x99, 0xCC, 0x21, 0x21, 0x21,
   0x09, 0x09, 0x09, 0x09, 0x09, 0x09, 0xFF, 0xFF, 0xFF, 0x0F, 0xD7, 0xFF, 0x69, 0xA2, 0xFF,
   0xD4, 0x80, 0xFF, 0xFF, 0x45, 0xF3, 0xFF, 0x61, 0x8B, 0xFF, 0x88, 0x33, 0xFF, 0x9C, 0x12,
   0xFA, 0xBC, 0x20, 0x9F, 0xE3, 0x0E, 0x2B, 0xF0, 0x35, 0x0C, 0xF0, 0xA4, 0x05, 0xFB, 0xFF,
   0x5E, 0x5E, 0x5E, 0x0D, 0x0D, 0x0D, 0x0D, 0x0D, 0x0D, 0xFF, 0xFF, 0xFF, 0xA6, 0xFC, 0xFF,
   0xB3, 0xEC, 0xFF, 0xDA, 0xAB, 0xEB, 0xFF, 0xA8, 0xF9, 0xFF, 0xAB, 0xB3, 0xFF, 0xD2, 0xB0,
   0xFF, 0xEF, 0xA6, 0xFF, 0xF7, 0x9C, 0xD7, 0xE8, 0x95, 0xA6, 0xED, 0xAF, 0xA2, 0xF2, 0xDA,
   0x99, 0xFF, 0xFC, 0xDD, 0xDD, 0xDD, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
};

void clear_frame(void) {
    memset(frame, 0, SCREEN_WIDTH * SCREEN_HEIGHT * 3);
}

void clear_bg_mask(void) {
    memset(opaque_bg_mask, false, SCREEN_WIDTH * SCREEN_HEIGHT);
}

void ppu_init(u8* chr, CartMetadata cart_metadata) {
    chr_rom = chr;
    clear_frame();
    scanlines = 0;
    dots = 0;
    frame_count = 0;
    cart = cart_metadata;
}

void ppu_free(void) {
    free(chr_rom);
}

u8 ppu_read_register(u16 addr) {
    switch (addr) {
        case 0x2002: {
            ppu_w = 0;
            u8 status = ppu_status;
            ppu_status &= ~PPU_STATUS_VBLANK;
            return status;
        }
        case 0x2004: {
            return oam[oam_addr];
        }
        case 0x2007: {
            u8 value = vram_internal_buffer;
            vram_internal_buffer = ppu_read(vram_addr);
            u16 increment = ppu_ctrl & 0b100 ? 32 : 1;
            vram_addr += increment;
            return value;
        }
        default: return 0;
    }
}

void ppu_transfer_oam(u16 start_addr) {
    memcpy(oam, ram + start_addr, 256);
    cycles += 513;
}

void ppu_write_register(u16 addr, u8 value) {
    switch (addr) {
        case 0x2000: {
            bool prev_nmi = ppu_ctrl & PPU_CTRL_NMI_ENABLE;
            ppu_ctrl = value;

            if (!prev_nmi && (value & PPU_CTRL_NMI_ENABLE) && (ppu_status & PPU_STATUS_VBLANK)) {
                nmi();
            }
            break;
        }
        case 0x2001: {
            ppu_mask = value;
            break;
        }
        case 0x2003: {
            oam_addr = value;
            break;
        }
        case 0x2004: {
            oam[oam_addr] = value;
            oam_addr++;
            break;
        }
        case 0x2005: {
            if (!ppu_w) {
                ppu_scroll_x = value;
                ppu_w = 1;
            } else {
                ppu_scroll_y = value;
                ppu_w = 0;
            }
            break;
        }
        case 0x2006: {
            if (ppu_w) {
                // low byte
                vram_addr = (vram_addr & 0xff00) | value;
                ppu_w = 0;
            } else {
                // high byte
                vram_addr = ((((u16)value) << 8) & 0xff00) | (vram_addr & 0xff);
                ppu_w = 1;
            }
            break;
        }
        case 0x2007: {
            ppu_write(vram_addr, value);
            u16 increment = ppu_ctrl & 0b100 ? 32 : 1;
            vram_addr += increment;
            break;
        }
    }
}

// https://www.nesdev.org/wiki/PPU_memory_map
u8 ppu_read(u16 addr) {
    if (addr < 0x2000) {
        return chr_rom[addr];
    }
    
    if (addr < 0x3f00) {
        return nametable[addr - 0x2000];
    }

    if (addr == 0x3f10 || addr == 0x3f14 || addr == 0x3f18 || addr == 0x3f1c) {
        return palette_table[addr - 0x3f10];
    }
    
    if (addr < 0x4000) {
        return palette_table[(addr - 0x3f00) & 31];
    }

    return 0;
}

void ppu_write(u16 addr, u8 value) {
    if (addr >= 0x2000 && addr < 0x3f00) {
        nametable[addr - 0x2000] = value;
    } else if (addr == 0x3f10 || addr == 0x3f14 || addr == 0x3f18 || addr == 0x3f1c) {
        palette_table[addr - 0x3f10] = value;
    } else if (addr < 0x4000) {
        palette_table[(addr - 0x3f00) & 31] = value;
    }
}

void set_pixel(usize x, usize y, u8 palette_color) {
    usize index = (y * SCREEN_WIDTH + x) * 3;

    if (index < SCREEN_WIDTH * SCREEN_HEIGHT * 3) {
        u8 r = COLOR_PALETTE[palette_color * 3];
        u8 g = COLOR_PALETTE[palette_color * 3 + 1];
        u8 b = COLOR_PALETTE[palette_color * 3 + 2];

        frame[index] = r;
        frame[index + 1] = g;
        frame[index + 2] = b;
    }
}

usize get_background_palette_index(usize tile_col, usize tile_row, usize nametable_offset) {
    usize attr_table_index = (tile_row / 4) * 8 + (tile_col / 4);
    // the attribute table is stored after the nametable (960 bytes)
    usize attr_table_byte = nametable[nametable_offset + 960 + attr_table_index];
    usize block_x = (tile_col % 4) / 2;
    usize block_y = (tile_row % 4) / 2;
    usize shift = block_y * 4 + block_x * 2;

    return ((attr_table_byte >> shift) & 0b11) * BYTES_PER_PALETTE;
}

void draw_background_tile(
    usize n,
    usize x,
    usize y,
    usize bank_offset,
    usize palette_idx,
    int shift_x,
    int min_x,
    int max_x
) {
    for (usize tile_y = 0; tile_y < 8; tile_y++) {
        u8 plane1 = chr_rom[bank_offset + n * 16 + tile_y];
        u8 plane2 = chr_rom[bank_offset + n * 16 + tile_y + 8];

        for (usize tile_x = 0; tile_x < 8; tile_x++) {
            u8 bit0 = plane1 & 1;
            u8 bit1 = plane2 & 1;
            u8 color_index = (u8)((bit1 << 1) | bit0);
            
            plane1 >>= 1;
            plane2 >>= 1;

            u8 palette_offset;
            bool is_universal_bg_color = color_index == 0;

            if (is_universal_bg_color) {
                palette_offset = palette_table[0];
            } else {
                palette_offset = palette_table[palette_idx + color_index];
            }

            int nametable_x = (int)x + ((int)(7 - (int)tile_x));

            if (nametable_x >= min_x && nametable_x < max_x) {
                usize screen_x = (usize)(shift_x + nametable_x);
                usize screen_y = y + tile_y;
                set_pixel(screen_x, screen_y, palette_offset);

                if (!is_universal_bg_color && screen_x >= 0 && screen_x < SCREEN_WIDTH) {
                    opaque_bg_mask[screen_y * SCREEN_WIDTH + screen_x] = true;
                }
            }
        }
    }
}

void render_nametable(usize nametable_offset, usize bank_offset, int shift_x, int min_x, int max_x) {
    bool draw_leftmost_tile = ppu_mask & PPU_MASK_SHOW_BACKGROUND_LEFTMOST;

    for (usize i = 0; i < TILES_PER_ROW * TILES_PER_COLUMN; i++) {
        u8 tile_x = i % TILES_PER_ROW;
        u8 tile_y = (u8)(i / TILES_PER_ROW);

        if (!draw_leftmost_tile && tile_x == 0) {
            continue;
        }

        u8 tile = nametable[nametable_offset + i];
        usize palette_index = get_background_palette_index(tile_x, tile_y, nametable_offset);

        draw_background_tile(tile, tile_x * 8, tile_y * 8, bank_offset, palette_index, shift_x, min_x, max_x);
    }
}

void render_row(usize y, usize nametable_offset, usize bank_offset, int shift_x, int min_x, int max_x) {
    bool draw_leftmost_tile = ppu_mask & PPU_MASK_SHOW_BACKGROUND_LEFTMOST;

    for (usize i = 0; i < TILES_PER_ROW; i++) {
        if (!draw_leftmost_tile && i == 0) {
            continue;
        }

        u8 tile = nametable[nametable_offset + y * TILES_PER_ROW + i];
        usize palette_index = get_background_palette_index(i, y, nametable_offset);

        draw_background_tile(tile, i * 8, y * 8, bank_offset, palette_index, shift_x, min_x, max_x);
    }
}

void draw_sprite_tile(
    usize n,
    usize x,
    usize y,
    usize bank_offset,
    usize palette_idx,
    bool flip_x,
    bool flip_y,
    bool behind_bg
) {
    for (usize tile_y = 0; tile_y < 8; tile_y++) {
        u8 plane1 = chr_rom[bank_offset + n * 16 + tile_y];
        u8 plane2 = chr_rom[bank_offset + n * 16 + tile_y + 8];

        for (usize tile_x = 0; tile_x < 8; tile_x++) {
            u8 bit0 = plane1 & 1;
            u8 bit1 = plane2 & 1;
            u8 color_index = (u8)((bit1 << 1) | bit0);
            
            plane1 >>= 1;
            plane2 >>= 1;

            if (color_index != 0) {
                u8 palette_offset = palette_table[palette_idx + color_index - 1];
                u8 flipped_x = (u8)(flip_x ? tile_x : 7 - tile_x);
                u8 flipped_y = (u8)(flip_y ? 7 - tile_y : tile_y);
                usize screen_x = x + flipped_x;
                usize screen_y = y + flipped_y;

                bool is_hidden = behind_bg && opaque_bg_mask[screen_y * SCREEN_WIDTH + screen_x];

                if (!is_hidden && screen_x < SCREEN_WIDTH) {
                    set_pixel(screen_x, screen_y, palette_offset);
                }
            }
        }
    }
}

void render_sprites(void) {
    // https://www.nesdev.org/wiki/PPU_OAM
    usize bank_offset = ppu_ctrl & 0b1000 ? 0x1000 : 0;
    bool draw_leftmost_tile = ppu_mask & 0b100;

    // sprites with lower OAM indices are drawn in front
    for (int i = 252; i >= 0; i -= 4) {
        u8 x = oam[i + 3];
        u8 y = oam[i] + 1;

        if (!draw_leftmost_tile && x == 0) {
            continue;
        }

        u8 tile = oam[i + 1];
        u8 attr = oam[i + 2];

        bool flip_x = attr & 0b01000000;
        bool flip_y = attr & 0b10000000;
        bool behind_bg = attr & 0b00100000;

        u8 palette_index = SPRITES_PALETTES_OFFSET + (attr & 0b11) * BYTES_PER_PALETTE;
        draw_sprite_tile(tile, x, y, bank_offset, palette_index, flip_x, flip_y, behind_bg);
    }
}

inline bool sprite_zero_hit(void) {
    u8 sprite0_y = oam[0];
    u8 sprite0_x = oam[3];
    return (ppu_mask & (PPU_MASK_SHOW_SPRITES | PPU_MASK_SHOW_BACKGROUND)) && (sprite0_y == scanlines) && (sprite0_x == dots);
}

bool ppu_step(usize cycles) {
    bool new_frame = false;

    for (usize i = 0; i < cycles; i++) {
        if (sprite_zero_hit()) {
            ppu_status |= PPU_STATUS_SPRITE0_HIT;
        }

        if (scanlines < 240 && dots == 256 && (ppu_mask & PPU_MASK_SHOW_BACKGROUND) && (scanlines & 7) == 0) {
            usize bank_offset = (ppu_ctrl & PPU_CTRL_BACKGROUND_PATTERN_TABLE) ? 0x1000 : 0;
            usize nametable1_offset, nametable2_offset;
        
            // vertical mirroring
            if (ppu_ctrl & PPU_CTRL_BASE_NAMETABLE_ADDR) {
                nametable1_offset = 0x400;
                nametable2_offset = 0x000;
            } else {
                nametable1_offset = 0x000;
                nametable2_offset = 0x400;
            }
        
            render_row(scanlines / 8, nametable1_offset, bank_offset, -((int)ppu_scroll_x), ppu_scroll_x, SCREEN_WIDTH);
            render_row(scanlines / 8, nametable2_offset, bank_offset, SCREEN_WIDTH - (int)ppu_scroll_x, 0, ppu_scroll_x);
        }

        dots++;

        if (dots > 340) {
            dots = 0;
            scanlines++;

            if (scanlines == 241) {
                new_frame = true;
                frame_count++;
                ppu_status |= PPU_STATUS_VBLANK;
                ppu_status &= ~PPU_STATUS_SPRITE0_HIT;

                if (ppu_ctrl & PPU_CTRL_NMI_ENABLE) {
                    nmi();
                }
            }
    
            if (scanlines > 261) {
                ppu_status &= ~(PPU_STATUS_VBLANK | PPU_STATUS_SPRITE0_HIT | PPU_STATUS_SPRITE_OVERFLOW);
                scanlines = 0;

                if (cart.reset_nametable_hack) {
                    // The status bar in Super Mario Bros flickers because of inaccurate scrolling handling
                    // this hack fixes this without requiring expensive scrolling computations
                    // see https://forums.nesdev.org/viewtopic.php?f=3&t=10762
                    ppu_ctrl &= ~PPU_CTRL_BASE_NAMETABLE_ADDR;
                }
            }
        }
    }

    return new_frame;
}

void ppu_render(void) {
    oam_addr = 0; // reset OAM address

    bool render_bg = ppu_mask & PPU_MASK_SHOW_BACKGROUND;
    bool render_sp = ppu_mask & PPU_MASK_SHOW_SPRITES;

    if (!render_bg) {
        clear_frame();
    }

    if (render_sp) {
        render_sprites();
    }
    
    clear_bg_mask();
}
