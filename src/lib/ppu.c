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

static inline void ppu_clear_frame(PPU* self) {
    memset(self->frame, 0, sizeof(self->frame));
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
    self->visible_scanline_sprites = 0;
    ppu_clear_frame(self);
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

static inline void ppu_increment_vram_addr(PPU* self) {
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

static inline u8 ppu_read_chr_rom(PPU* self, u16 addr) {
    return self->mapper->read(self->mapper, addr);
}

static inline void ppu_write_chr_rom(PPU* self, u16 addr, u8 value) {
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

static inline void ppu_scroll_copy_x(PPU* self) {
    // copy all bits related to horizontal position from t to v:
    // v: ....A.. ...BCDEF <- t: ....A.. ...BCDEF
    self->v_reg = (self->v_reg & 0xFBE0) | (self->t_reg & 0x041F);
}

static inline void ppu_scroll_copy_y(PPU* self) {
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

typedef struct {
    u8 palette_color;
    u8 is_opaque;
} BackgroundPixelData;

BackgroundPixelData ppu_get_background_pixel(PPU* self) {
    BackgroundPixelData pixel_data = {0};
    usize x = (usize)(self->dots - 1);

    if ((self->mask_reg & PPU_MASK_SHOW_BACKGROUND_LEFTMOST) || x > 7) {
        usize y = (usize)(self->scanlines);
        u8 palette_index = 0;

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
                pixel_data.is_opaque = true;
            }
        }

        pixel_data.palette_color = self->palette_table[palette_index] & 63; // Mask to 6 bits
    }

    return pixel_data;
}

typedef struct {
    u8 palette_color;
    u8 tile_index;
    bool behind_background;
    bool is_opaque;
} SpritePixelData;

SpritePixelData ppu_get_sprite_pixel(PPU* self) {
    SpritePixelData pixel_data = {0};
    u16 x = (u16)(self->dots - 1);

    if ((self->mask_reg & PPU_MASK_SHOW_SPRITES) && ((self->mask_reg & PPU_MASK_SHOW_SPRITES_LEFTMOST) || x > 7)) {
        for (usize i = 0; i < self->visible_scanline_sprites; i++) {
            SpriteData* s = &self->scanline_sprites[i];

            if (x >= s->x && x < s->x + 8) {
                u8 color_index = s->chr[x - s->x];

                if (color_index != 0) {
                    pixel_data.is_opaque = true;
                    usize palette_index = SPRITES_PALETTES_OFFSET + s->palette_index * BYTES_PER_PALETTE + color_index - 1;
                    pixel_data.palette_color = self->palette_table[palette_index] & 63;
                    pixel_data.tile_index = s->tile_index;
                    pixel_data.behind_background = s->behind_background;
                    break;
                }
            }
        }
    }

    return pixel_data;
}

void ppu_render_pixel(PPU* self) {
    usize x = (usize)(self->dots - 1);
    usize y = (usize)self->scanlines;
    BackgroundPixelData bg = ppu_get_background_pixel(self);
    SpritePixelData sprite = ppu_get_sprite_pixel(self);

    u8 palette_color;

    if (!bg.is_opaque && !sprite.is_opaque) {
        palette_color = self->palette_table[0] & 63; // Transparent pixel
    } else if (sprite.is_opaque && (!sprite.behind_background || !bg.is_opaque)) {
        palette_color = sprite.palette_color;
    } else {
        palette_color = bg.palette_color;
    }

    ppu_set_pixel(self, x, y, palette_color);

    // Sprite 0 hit detection
    if (sprite.tile_index == 0 && sprite.is_opaque && bg.is_opaque && x < 255 && !(self->status_reg & PPU_STATUS_SPRITE0_HIT)) {
        self->status_reg |= PPU_STATUS_SPRITE0_HIT;
    }
}

void ppu_fetch_next_scanline_sprites(PPU* self) {
    u8 count = 0; // number of visible sprites on the current scanline
    u16 sprite_size = self->ctrl_reg & PPU_CTRL_SPRITE_SIZE ? 16 : 8;

    for (usize i = 0; i < 64; i++) {
        usize offset = i * 4;
        u8 y = self->oam[offset];

        if (self->scanlines >= y && self->scanlines < y + sprite_size) {
            u8 row = (u8)(self->scanlines - y);
            u8 tile_idx = self->oam[offset + 1];
            u8 attr = self->oam[offset + 2];
            u8 palette_idx = attr & 0b11;
            bool behind_background = attr & 0b00100000;
            bool flip_horizontally = attr & 0b01000000;
            bool flip_vertically = attr & 0b10000000;
            u8 x = self->oam[offset + 3];

            u16 chr_bank = 0;

            if (sprite_size == 8) {
                chr_bank = self->ctrl_reg & PPU_CTRL_SPRITE_PATTERN_TABLE ? 0x1000 : 0;

                if (flip_vertically) {
                    row = 7 - row;
                }
            } else {
                chr_bank = (tile_idx & 1) * 0x1000;
                tile_idx &= 0xFE;

                if (flip_vertically) {
                    row = 15 - row;
                }

                if (row > 7) {
                    row -= 8;
                    tile_idx++;
                }
            }

            if (count < 8) {
                u16 tile_offset = chr_bank + tile_idx * 16 + row;
                u8 chr_low = ppu_read_chr_rom(self, tile_offset);
                u8 chr_high = ppu_read_chr_rom(self, tile_offset + 8);
                u8 chr[8] = {0};

                for (usize j = 0; j < 8; j++) {
                    u8 mask = (u8)(1 << (flip_horizontally ? j : 7 - j));
                    u8 p1 = (chr_low & mask) != 0;
                    u8 p2 = (chr_high & mask) != 0;
                    u8 pattern = (u8)((p2 << 1) | p1);
                    chr[j] = pattern;
                }

                SpriteData* sprite_data = &self->scanline_sprites[count];
                sprite_data->x = (u16)x;
                sprite_data->tile_index = (u8)i;
                sprite_data->palette_index = palette_idx;
                sprite_data->behind_background = behind_background;
                sprite_data->chr[0] = chr[0];
                sprite_data->chr[1] = chr[1];
                sprite_data->chr[2] = chr[2];
                sprite_data->chr[3] = chr[3];
                sprite_data->chr[4] = chr[4];
                sprite_data->chr[5] = chr[5];
                sprite_data->chr[6] = chr[6];
                sprite_data->chr[7] = chr[7];

                count++;
            } else {
                // TODO: implement sprite overflow hardware bug
                self->status_reg |= PPU_STATUS_SPRITE_OVERFLOW;
                break;
            }
        }
    }

    self->visible_scanline_sprites = count;
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

        if (show_background) {
            if (visible_cycle && visible_line) {
                ppu_render_pixel(self);
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


        if (show_sprites && self->dots == 257) {
            if (visible_line) {
                ppu_fetch_next_scanline_sprites(self);
            } else {
                // clear secondary OAM
                self->visible_scanline_sprites = 0;
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
