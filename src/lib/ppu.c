#include "ppu.h"
#include <stdlib.h>

#define SPRITES_PALETTES_OFFSET 0x11
#define BYTES_PER_PALETTE 4
#define TILES_PER_ROW 32
#define TILES_PER_COLUMN 30

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

inline void ppu_clear_frame(PPU* self) {
    memset(self->frame, 0, sizeof(self->frame));
}

inline void ppu_clear_bg_mask(PPU* self) {
    memset(self->opaque_bg_mask, false, sizeof(self->opaque_bg_mask));
}

void ppu_reset(PPU* self) {
    self->dots = 340;
    self->scanlines = 240;
    self->frame_count = 0;
    ppu_write_register(self, 0x2000, 0);
    ppu_write_register(self, 0x2001, 0);
}

void ppu_init(PPU* self, Cart *cart, Mapper* mapper) {
    self->scanlines = 0;
    self->dots = 0;
    self->frame_count = 0;
    self->cart = cart;
    self->mapper = mapper;
    memset(self->nametable, 0, sizeof(self->nametable));
    memset(self->palette_table, 0, sizeof(self->palette_table));
    memset(self->oam, 0, sizeof(self->oam));
    self->ctrl_reg = 0;
    self->mask_reg = 0;
    self->status_reg = 0;
    self->oam_addr_reg = 0;
    self->scroll_x = 0;
    self->scroll_y = 0;
    self->v_reg = 0;
    self->t_reg = 0;
    self->x_reg = 0;
    self->data_buffer = 0;
    self->write_toggle = false;
    self->oam_dma = 0;
    self->nmi_edge_detector = false;
    self->should_trigger_nmi = false;
    self->nmi_triggered = false;
    self->nametable_byte = 0;
    self->attribute_byte = 0;
    self->pattern_low_byte = 0;
    self->pattern_high_byte = 0;
    self->pattern_data_shift_registers[0] = 0;
    self->pattern_data_shift_registers[1] = 0;
    self->attribute_data_latches[0] = false;
    self->attribute_data_latches[1] = false;
    self->attribute_data_shift_registers[0] = 0;
    self->attribute_data_shift_registers[1] = 0;
    ppu_clear_frame(self);
    ppu_clear_bg_mask(self);
    ppu_reset(self);
}

void ppu_free(PPU* self) {}

void ppu_detect_nmi_edge(PPU* self) {
    bool new_state = (self->ctrl_reg & PPU_CTRL_NMI_ENABLE) && (self->status_reg & PPU_STATUS_VBLANK);

    if (!self->nmi_edge_detector && new_state) {
        self->should_trigger_nmi = true;
    }

    self->nmi_edge_detector = new_state;
}

inline void ppu_increment_vram_addr(PPU* self) {
    self->v_reg += self->ctrl_reg & PPU_CTRL_VRAM_INCREMENT ? 32 : 1;
}

u8 ppu_read_register(PPU* self, u16 addr) {
    switch (addr) {
        case 0x2002: {
            self->write_toggle = false;
            u8 status = self->status_reg;
            self->status_reg &= ~PPU_STATUS_VBLANK;
            ppu_detect_nmi_edge(self);
            return status;
        }
        case 0x2004: {
            return self->oam[self->oam_addr_reg];
        }
        case 0x2007: {
            u8 value = self->data_buffer;
            self->data_buffer = ppu_read(self, self->v_reg);
            ppu_increment_vram_addr(self);
            return value;
        }
        default: return 0;
    }
}

void ppu_write_register(PPU* self, u16 addr, u8 value) {
    switch (addr) {
        case 0x2000: {
            self->ctrl_reg = value;
            // t: ...GH.. ........ <- d: ......GH
            self->t_reg = (u16)(self->t_reg & 0xF3FF) | (u16)((u16)(value & 0b11) << 10);
            ppu_detect_nmi_edge(self);
            break;
        }
        case 0x2001: {
            self->mask_reg = value;
            break;
        }
        case 0x2003: {
            self->oam_addr_reg = value;
            break;
        }
        case 0x2004: {
            self->oam[self->oam_addr_reg] = value;
            self->oam_addr_reg++;
            break;
        }
        case 0x2005: {
            if (!self->write_toggle) {
                self->scroll_x = value;
                // t: ....... ...ABCDE <- d: ABCDE...
                // x:              FGH <- d: .....FGH
                self->t_reg = (self->t_reg & 0xFFE0) | ((u16)value >> 3);
                self->x_reg = value & 0b111;
            } else {
                self->scroll_y = value;
                // t: FGH..AB CDE..... <- d: ABCDEFGH
                self->t_reg = (u16)(self->t_reg & 0x8FFF) | (u16)(((u16)value & 0b111) << 12);
                self->t_reg = (u16)(self->t_reg & 0xFC1F) | (u16)(((u16)value & 0b11111000) << 2);
            }

            self->write_toggle = !self->write_toggle;
            break;
        }
        case 0x2006: {
            if (!self->write_toggle) {
                // t: .CDEFGH ........ <- d: ..CDEFGH
                // t: Z...... ........ <- 0 (bit Z is cleared)
                self->t_reg = ((self->t_reg & 0x80FF) | ((u16)(value & 0b111111) << 8)) & 0x7FFF;
            } else {
                // t: ....... ABCDEFGH <- d: ABCDEFGH
                // v: <...all bits...> <- t: <...all bits...>
                self->t_reg = (self->t_reg & 0xFF00) | ((u16)value);
                self->v_reg = self->t_reg;
            }

            self->write_toggle = !self->write_toggle;
            break;
        }
        case 0x2007: {
            ppu_write(self, self->v_reg, value);
            ppu_increment_vram_addr(self);
            break;
        }
    }
}

u16 ppu_nametable_mirrored_addr(PPU* self, u16 addr) {
    addr &= 0x2fff;

    switch (self->cart->header.mirroring) {
        case NT_MIRRORING_HORIZONTAL:
            if (addr >= 0x2000 && addr <= 0x23FF) {
                return addr - 0x2000;                 // A
            } else if (addr >= 0x2400 && addr <= 0x27FF) {
                return addr - 0x2400;                 // A
            } else if (addr >= 0x2800 && addr <= 0x2BFF) {
                return addr - 0x2800 + 1024;          // B
            } else {
                return addr - 0x2C00 + 1024;          // B
            }
        case NT_MIRRORING_VERTICAL:
            if (addr >= 0x2000 && addr <= 0x23FF) {
                return addr - 0x2000;                 // A
            } else if (addr >= 0x2400 && addr <= 0x27FF) {
                return addr - 0x2400 + 1024;          // B
            } else if (addr >= 0x2800 && addr <= 0x2BFF) {
                return addr - 0x2800;                 // A
            } else {
                return addr - 0x2C00 + 1024;          // B
            }
        case NT_MIRRORING_ONE_SCREEN_LOWER_BANK:
            if (addr >= 0x2000 && addr <= 0x23FF) {
                return addr - 0x2000;                 // A
            } else if (addr >= 0x2400 && addr <= 0x27FF) {
                return addr - 0x2400;                 // A
            } else if (addr >= 0x2800 && addr <= 0x2BFF) {
                return addr - 0x2800;                 // A
            } else {
                return addr - 0x2C00;                 // A
            }
        case NT_MIRRORING_ONE_SCREEN_UPPER_BANK:
            if (addr >= 0x2000 && addr <= 0x23FF) {
                return addr - 0x2000 + 1024;          // B
            } else if (addr >= 0x2400 && addr <= 0x27FF) {
                return addr - 0x2400 + 1024;          // B
            } else if (addr >= 0x2800 && addr <= 0x2BFF) {
                return addr - 0x2800 + 1024;          // B
            } else {
                return addr - 0x2C00 + 1024;          // B
            }
        case NT_MIRRORING_FOUR_SCREEN:
            return addr - 0x2000;
    }

    #ifdef NESC_VERBOSE
    printf("Invalid nametable address: %04X\n", addr);
    #endif
    exit(1);

    return 0;
}

inline u8 ppu_read_chr_rom(PPU* self, u16 addr) {
    return self->mapper->read(self->mapper, addr);
}

inline void ppu_write_chr_rom(PPU* self, u16 addr, u8 value) {
    self->mapper->write(self->mapper, addr, value);
}

// https://www.nesdev.org/wiki/PPU_memory_map
u8 ppu_read(PPU* self, u16 addr) {
    if (addr < 0x2000) {
        return ppu_read_chr_rom(self, addr);
    }

    if (addr < 0x3f00) {
        return self->nametable[ppu_nametable_mirrored_addr(self, addr)];
    }

    if (addr == 0x3f10 || addr == 0x3f14 || addr == 0x3f18 || addr == 0x3f1c) {
        return self->palette_table[(addr - 0x3f10) & 31];
    }

    if (addr < 0x4000) {
        return self->palette_table[(addr - 0x3f00) & 31];
    }

    return 0;
}

void ppu_write(PPU* self, u16 addr, u8 value) {
    if (addr < 0x2000) {
        ppu_write_chr_rom(self, addr, value);
    } else if (addr >= 0x2000 && addr < 0x3f00) {
        self->nametable[ppu_nametable_mirrored_addr(self, addr)] = value;
    } else if (addr == 0x3f10 || addr == 0x3f14 || addr == 0x3f18 || addr == 0x3f1c) {
        self->palette_table[(addr - 0x3f10) & 31] = value;
    } else if (addr < 0x4000) {
        self->palette_table[(addr - 0x3f00) & 31] = value;
    }
}

void ppu_set_pixel(PPU* self, usize x, usize y, u8 palette_color) {
    usize index = (y * SCREEN_WIDTH + x) * 3;

    if (index < SCREEN_WIDTH * SCREEN_HEIGHT * 3) {
        usize offset = palette_color * 3;
        u8 r = COLOR_PALETTE[offset];
        u8 g = COLOR_PALETTE[offset + 1];
        u8 b = COLOR_PALETTE[offset + 2];

        self->frame[index] = r;
        self->frame[index + 1] = g;
        self->frame[index + 2] = b;
    }
}

usize ppu_get_background_palette_index(PPU* self, usize tile_col, usize tile_row, usize nametable_offset) {
    usize attr_table_index = (tile_row / 4) * 8 + (tile_col / 4);
    // the attribute table is stored after the nametable (960 bytes)
    usize attr_table_byte = self->nametable[nametable_offset + 960 + attr_table_index];
    usize block_x = (tile_col % 4) / 2;
    usize block_y = (tile_row % 4) / 2;
    usize shift = block_y * 4 + block_x * 2;

    return ((attr_table_byte >> shift) & 0b11) * BYTES_PER_PALETTE;
}

void ppu_draw_background_tile(
    PPU* self,
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
        usize chr_rom_offset = bank_offset + n * 16 + tile_y;
        u8 plane1 = ppu_read_chr_rom(self, (u16)chr_rom_offset);
        u8 plane2 = ppu_read_chr_rom(self, (u16)(chr_rom_offset + 8));

        for (usize tile_x = 0; tile_x < 8; tile_x++) {
            u8 bit0 = plane1 & 1;
            u8 bit1 = plane2 & 1;
            u8 color_index = (u8)((bit1 << 1) | bit0);

            plane1 >>= 1;
            plane2 >>= 1;

            u8 palette_offset;
            bool is_universal_bg_color = color_index == 0;

            if (is_universal_bg_color) {
                palette_offset = self->palette_table[0];
            } else {
                palette_offset = self->palette_table[palette_idx + color_index];
            }

            int nametable_x = (int)x + ((int)(7 - (int)tile_x));

            if (nametable_x >= min_x && nametable_x < max_x) {
                usize screen_x = (usize)(shift_x + nametable_x);
                usize screen_y = y + tile_y;
                ppu_set_pixel(self, screen_x, screen_y, palette_offset);

                if (!is_universal_bg_color && screen_x >= 0 && screen_x < SCREEN_WIDTH) {
                    self->opaque_bg_mask[screen_y * SCREEN_WIDTH + screen_x] = true;
                }
            }
        }
    }
}

void ppu_render_row(PPU* self, usize y, usize nametable_offset, usize bank_offset, int shift_x, int min_x, int max_x) {
    usize start_x = !(self->mask_reg & PPU_MASK_SHOW_BACKGROUND_LEFTMOST);

    for (usize i = start_x; i < TILES_PER_ROW; i++) {
        u8 tile = self->nametable[nametable_offset + y * TILES_PER_ROW + i];
        usize palette_index = ppu_get_background_palette_index(self, i, y, nametable_offset);
        ppu_draw_background_tile(self, tile, i * 8, y * 8, bank_offset, palette_index, shift_x, min_x, max_x);
    }
}

void ppu_draw_sprite_tile(
    PPU* self,
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
        usize chr_rom_offset = bank_offset + n * 16 + tile_y;
        u8 plane1 = ppu_read_chr_rom(self, (u16)chr_rom_offset);
        u8 plane2 = ppu_read_chr_rom(self, (u16)(chr_rom_offset + 8));

        for (usize tile_x = 0; tile_x < 8; tile_x++) {
            u8 bit0 = plane1 & 1;
            u8 bit1 = plane2 & 1;
            u8 color_index = (u8)((bit1 << 1) | bit0);

            plane1 >>= 1;
            plane2 >>= 1;

            if (color_index != 0) {
                u8 palette_offset = self->palette_table[palette_idx + color_index - 1];
                u8 flipped_x = (u8)(flip_x ? tile_x : 7 - tile_x);
                u8 flipped_y = (u8)(flip_y ? 7 - tile_y : tile_y);
                usize screen_x = x + flipped_x;
                usize screen_y = y + flipped_y;

                bool is_hidden = behind_bg && self->opaque_bg_mask[screen_y * SCREEN_WIDTH + screen_x];

                if (!is_hidden && screen_x < SCREEN_WIDTH) {
                    ppu_set_pixel(self, screen_x, screen_y, palette_offset);
                }
            }
        }
    }
}

void ppu_render_sprites(PPU* self) {
    // https://www.nesdev.org/wiki/PPU_OAM
    usize bank_offset = self->ctrl_reg & PPU_CTRL_SPRITE_PATTERN_TABLE ? 0x1000 : 0;
    bool draw_leftmost_tile = self->mask_reg & PPU_MASK_SHOW_SPRITES_LEFTMOST;

    // sprites with lower OAM indices are drawn in front
    for (int i = 252; i >= 0; i -= 4) {
        usize y = (usize)self->oam[i] + 1;
        u8 tile = self->oam[i + 1];
        u8 attr = self->oam[i + 2];
        usize x = (usize)self->oam[i + 3];

        if ((!draw_leftmost_tile && x == 0) || (y == SCREEN_HEIGHT)) {
            continue;
        }

        bool flip_x = attr & 0b01000000;
        bool flip_y = attr & 0b10000000;
        bool behind_bg = attr & 0b00100000;

        u8 palette_index = SPRITES_PALETTES_OFFSET + (attr & 0b11) * BYTES_PER_PALETTE;
        ppu_draw_sprite_tile(self, tile, x, y, bank_offset, palette_index, flip_x, flip_y, behind_bg);
    }
}

inline bool ppu_sprite_zero_hit(PPU* self) {
    u8 sprite0_y = self->oam[0];
    u8 sprite0_x = self->oam[3];
    return (self->mask_reg & (PPU_MASK_SHOW_SPRITES | PPU_MASK_SHOW_BACKGROUND)) && (sprite0_y + 6 == self->scanlines) && (sprite0_x + 1 == self->dots);
}

void ppu_scroll_increment_coarse_x(PPU* self) {
    // if we are at the end of the nametable (32 tiles wide)
    if ((self->v_reg & PPU_V_COARSE_X_SCROLL) == 31) {
        self->v_reg &= ~PPU_V_COARSE_X_SCROLL; // set coarse X to 0
        self->v_reg ^= 0x0400; // switch horizontal nametable
    } else {
        self->v_reg += 1; // scroll to the right by 1 tile
    }
}

void ppu_scroll_increment_y(PPU* self) {
    // if fine y < 7
    if ((self->v_reg & PPU_V_FINE_Y_SCROLL) != PPU_V_FINE_Y_SCROLL) {
        self->v_reg += 0x1000; // increment fine Y
    } else {
        self->v_reg &= ~PPU_V_FINE_Y_SCROLL; // fine Y = 0
        u16 coarse_y = (self->v_reg & PPU_V_COARSE_Y_SCROLL) >> 5;
        // if we are at the end of the nametable (30 tiles high)
        if (coarse_y == 29) {
            coarse_y = 0;
            self->v_reg ^= 0x0800; // switch vertical nametable
        } else if (coarse_y == 31) {
            coarse_y = 0; // coarse Y = 0, nametable not switched
        } else {
            coarse_y += 1; // increment coarse Y
        }

        // put coarse Y back into v
        self->v_reg = (u16)((self->v_reg & ~PPU_V_COARSE_Y_SCROLL) | (coarse_y << 5));
    }
}

inline void ppu_scroll_copy_x(PPU* self) {
    // copy all bits related to horizontal position from t to v:
    // v: ....A.. ...BCDEF <- t: ....A.. ...BCDEF
    self->v_reg = (self->v_reg & 0xFBE0) | (self->t_reg & 0x041F);
}

inline void ppu_scroll_copy_y(PPU* self) {
    // copy the vertical bits from t to v
    // v: GHIA.BC DEF..... <- t: GHIA.BC DEF.....
    self->v_reg = (self->v_reg & 0x841F) | (self->t_reg & 0x7BE0);
}

void ppu_fetch_nametable_byte(PPU* self) {
    // Tile and attribute fetching
    // https://www.nesdev.org/wiki/PPU_scrolling
    u16 tile_addr = 0x2000 | (self->v_reg & 0x0FFF);
    u16 mirrored_addr = ppu_nametable_mirrored_addr(self, tile_addr);
    self->nametable_byte = self->nametable[mirrored_addr];
}

void ppu_fetch_attribute_byte(PPU* self) {
    u16 attribute_addr = 0x23C0 | (self->v_reg & 0x0C00) | ((self->v_reg >> 4) & 0x38) | ((self->v_reg >> 2) & 0x07);
    u16 mirrored_addr = ppu_nametable_mirrored_addr(self, attribute_addr);
    usize shift = ((self->v_reg >> 4) & 4) | (self->v_reg & 2);
    self->attribute_byte = (self->nametable[mirrored_addr] >> shift) & 0b11;
}

void ppu_store_tile_data(PPU* self) {
    self->pattern_data_shift_registers[0] = (u16)(self->pattern_data_shift_registers[0] | self->pattern_low_byte);
    self->pattern_data_shift_registers[1] = (u16)(self->pattern_data_shift_registers[1] | self->pattern_high_byte);
    self->attribute_data_latches[0] = self->attribute_byte & 1;
    self->attribute_data_latches[1] = self->attribute_byte & 2;
}

void ppu_fetch_pattern_bytes(PPU* self) {
    u16 bank_offset = self->ctrl_reg & PPU_CTRL_BACKGROUND_PATTERN_TABLE ? 0x1000 : 0;
    u8 tile = self->nametable_byte;
    u8 fine_y = (u8)((self->v_reg & PPU_V_FINE_Y_SCROLL) >> 12);
    u16 offset = bank_offset + tile * 16 + (u16)fine_y;
    self->pattern_low_byte = ppu_read_chr_rom(self, offset);
    self->pattern_high_byte = ppu_read_chr_rom(self, offset + 8);
}

void ppu_render_background_pixel(PPU* self) {
    usize x = (usize)(self->dots - 1);
    usize y = (usize)(self->scanlines);
    u8 palette_index = 0;
    bool is_opaque = false;

    if (self->mask_reg & PPU_MASK_SHOW_BACKGROUND) {
        u8 pattern0 = (u8)(self->pattern_data_shift_registers[0] >> (15 - self->x_reg)) & 1;
        u8 pattern1 = (u8)(self->pattern_data_shift_registers[1] >> (15 - self->x_reg)) & 1;
        u8 pattern = (u8)((pattern1 << 1) | pattern0);
        u8 attr0 = (u8)(self->attribute_data_shift_registers[0] >> (7 - self->x_reg)) & 1;
        u8 attr1 = (u8)(self->attribute_data_shift_registers[1] >> (7 - self->x_reg)) & 1;
        u8 attr = (u8)((attr1 << 1) | attr0);
        u8 pixel_attribute_and_pattern = (u8)((attr << 2) | pattern);

        if (pattern != 0) { // if pixel is not transparent
            palette_index = pixel_attribute_and_pattern; // Use AAPP as the offset (0-15)
            is_opaque = true;
        }
    }

    u8 palette_color = self->palette_table[palette_index] & 63; // Mask to 6 bits
    ppu_set_pixel(self, x, y, palette_color);
    self->opaque_bg_mask[y * SCREEN_WIDTH + x] = is_opaque;
}

void ppu_tick(PPU* self) {
    if (self->should_trigger_nmi && (self->ctrl_reg & PPU_CTRL_NMI_ENABLE) && (self->status_reg & PPU_STATUS_VBLANK)) {
        self->should_trigger_nmi = false;
        self->nmi_triggered = true;
    }

    bool rendering_enabled = (self->mask_reg & PPU_MASK_SHOW_BACKGROUND) || (self->mask_reg & PPU_MASK_SHOW_SPRITES);

    if (rendering_enabled && (self->frame_count & 1) && self->scanlines == 261 && self->dots == 339) {
        // skip cycle 339 of pre-render scanline
        self->dots = 0;
        self->scanlines = 0;
        self->frame_count++;
        return;
    }

    self->dots++;

    if (self->dots > 340) {
        self->dots = 0;
        self->scanlines++;

        if (self->scanlines > 261) {
            self->scanlines = 0;
            self->frame_count++;
        }
    }
}

bool ppu_step(PPU* self, usize cycles) {
    bool new_frame = false;

    for (usize i = 0; i < cycles; i++) {
        ppu_tick(self);

        bool show_background = self->mask_reg & PPU_MASK_SHOW_BACKGROUND;
        bool show_sprites = self->mask_reg & PPU_MASK_SHOW_SPRITES;
        bool rendering_enabled = show_background || show_sprites;
        bool pre_render_line = self->scanlines == 261;
        bool visible_line = self->scanlines < 240;
        bool pre_fetch_cycle = self->dots >= 321 && self->dots <= 336; // fetch first 2 tiles of the next line
        bool visible_cycle = self->dots >= 1 && self->dots <= 256;
        bool fetch_cycle = pre_fetch_cycle || visible_cycle;
        bool render_line = pre_render_line || visible_line;

        if (ppu_sprite_zero_hit(self)) {
            self->status_reg |= PPU_STATUS_SPRITE0_HIT;
        }

        if (show_background) {
            if (visible_cycle && visible_line) {
                ppu_render_background_pixel(self);
            }

            if (render_line && fetch_cycle) {
                self->attribute_data_shift_registers[0] <<= 1;
                self->attribute_data_shift_registers[1] <<= 1;
                self->attribute_data_shift_registers[0] |= (u16)self->attribute_data_latches[0];
                self->attribute_data_shift_registers[1] |= (u16)self->attribute_data_latches[1];
                self->pattern_data_shift_registers[0] <<= 1;
                self->pattern_data_shift_registers[1] <<= 1;

                switch (self->dots & 7) {
                    case 1: {
                        ppu_fetch_nametable_byte(self);
                        break;
                    }
                    case 3: {
                        ppu_fetch_attribute_byte(self);
                        break;
                    }
                    case 7: {
                        ppu_fetch_pattern_bytes(self);
                        break;
                    }
                    case 0: {
                        ppu_store_tile_data(self);
                        break;
                    }
                    default: {
                        break;
                    }
                }
            }

            if (pre_render_line && self->dots >= 280 && self->dots <= 304) {
                ppu_scroll_copy_y(self);
            }

            if (render_line) {
                if (fetch_cycle && (self->dots & 7) == 0) {
                    ppu_scroll_increment_coarse_x(self);
                }

                if (self->dots == 256) {
                    ppu_scroll_increment_y(self);
                } else if (self->dots == 257) {
                    ppu_scroll_copy_x(self);
                }
            }
        }

        if (self->dots == 1) {
            if (self->scanlines == 241) {
                new_frame = true;
                self->frame_count++;
                self->status_reg |= PPU_STATUS_VBLANK;
                ppu_detect_nmi_edge(self);
            } else if (pre_render_line) {
                self->status_reg &= ~(PPU_STATUS_VBLANK | PPU_STATUS_SPRITE0_HIT | PPU_STATUS_SPRITE_OVERFLOW);
                ppu_detect_nmi_edge(self);
            }
        }
    }

    return new_frame;
}

void ppu_render(PPU* self) {
    self->oam_addr_reg = 0; // reset OAM address
    bool render_bg = self->mask_reg & PPU_MASK_SHOW_BACKGROUND;
    bool render_sp = self->mask_reg & PPU_MASK_SHOW_SPRITES;

    if (!render_bg) {
        ppu_clear_frame(self);
    }

    if (render_sp) {
        ppu_render_sprites(self);
    }

    ppu_clear_bg_mask(self);
}
