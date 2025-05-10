#include "ppu.h"

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

NametableOffsets ppu_get_nametable_offsets(PPU* self, u8 base_nametable);

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
    self->vram_addr = 0;
    self->vram_internal_buffer = 0;
    self->write_toggle = false;
    self->oam_dma = 0;
    self->nametable_offsets = ppu_get_nametable_offsets(self, self->ctrl_reg & PPU_CTRL_BASE_NAMETABLE_ADDR);
    self->nmi_edge_detector = false;
    self->should_trigger_nmi = false;
    self->nmi_triggered = false;
    ppu_clear_frame(self);
    ppu_clear_bg_mask(self);
}

void ppu_free(PPU* self) {}

void ppu_detect_nmi_edge(PPU* self) {
    bool new_state = (self->ctrl_reg & PPU_CTRL_NMI_ENABLE) && (self->status_reg & PPU_STATUS_VBLANK);

    if (!self->nmi_edge_detector && new_state) {
        self->should_trigger_nmi = true;
    }

    self->nmi_edge_detector = new_state;
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
            u8 value = self->vram_internal_buffer;
            self->vram_internal_buffer = ppu_read(self, self->vram_addr);
            self->vram_addr += self->ctrl_reg & PPU_CTRL_VRAM_INCREMENT ? 32 : 1;
            return value;
        }
        default: return 0;
    }
}

void ppu_write_register(PPU* self, u16 addr, u8 value) {
    switch (addr) {
        case 0x2000: {
            u8 prev_base_nametable = self->ctrl_reg & PPU_CTRL_BASE_NAMETABLE_ADDR;
            self->ctrl_reg = value;

            if ((value & PPU_CTRL_BASE_NAMETABLE_ADDR) != prev_base_nametable) {
                self->nametable_offsets = ppu_get_nametable_offsets(self, self->ctrl_reg & PPU_CTRL_BASE_NAMETABLE_ADDR);
            }

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
            if (self->write_toggle) {
                self->scroll_y = value;
            } else {
                self->scroll_x = value;
            }

            self->write_toggle = !self->write_toggle;
            break;
        }
        case 0x2006: {
            if (self->write_toggle) {
                // low byte
                self->vram_addr = (self->vram_addr & 0xff00) | value;
            } else {
                // high byte
                self->vram_addr = ((((u16)value) << 8) & 0xff00) | (self->vram_addr & 0xff);
            }

            self->write_toggle = !self->write_toggle;
            break;
        }
        case 0x2007: {
            ppu_write(self, self->vram_addr, value);
            self->vram_addr += self->ctrl_reg & PPU_CTRL_VRAM_INCREMENT ? 32 : 1;
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

    printf("Invalid nametable address: %04X\n", addr);
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
        u8 r = COLOR_PALETTE[palette_color * 3];
        u8 g = COLOR_PALETTE[palette_color * 3 + 1];
        u8 b = COLOR_PALETTE[palette_color * 3 + 2];

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
    return (self->mask_reg & (PPU_MASK_SHOW_SPRITES | PPU_MASK_SHOW_BACKGROUND)) && (sprite0_y == self->scanlines) && (sprite0_x == self->dots);
}

NametableOffsets ppu_get_nametable_offsets(PPU* self, u8 base_nametable) {
    usize nametable1_offset, nametable2_offset;

    switch (self->cart->header.mirroring) {
        case NT_MIRRORING_VERTICAL: {
            if ((base_nametable & 1) == 0) {
                nametable1_offset = 0x000;
                nametable2_offset = 0x400;
            } else {
                nametable1_offset = 0x400;
                nametable2_offset = 0x000;
            }
            break;
        }
        case NT_MIRRORING_HORIZONTAL: {
            switch (base_nametable) {
                case 0:
                case 1:
                    nametable1_offset = 0x000;
                    nametable2_offset = 0x400;
                    break;
                case 2:
                case 3:
                    nametable1_offset = 0x400;
                    nametable2_offset = 0x000;
                    break;
            }
            break;
        }
        case NT_MIRRORING_ONE_SCREEN_LOWER_BANK:
            nametable1_offset = 0x000;
            nametable2_offset = 0x000;
            break;
        case NT_MIRRORING_ONE_SCREEN_UPPER_BANK:
            nametable1_offset = 0x400;
            nametable2_offset = 0x400;
            break;
        case NT_MIRRORING_FOUR_SCREEN:
            switch (base_nametable) {
                case 0:
                    nametable1_offset = 0x000;
                    nametable2_offset = 0x400;
                    break;
                case 1:
                    nametable1_offset = 0x400;
                    nametable2_offset = 0x800;
                    break;
                case 2:
                    nametable1_offset = 0x800;
                    nametable2_offset = 0xc00;
                    break;
                case 3:
                    nametable1_offset = 0xc00;
                    nametable2_offset = 0x800;
                    break;
            }
            break;
    }

    return (NametableOffsets) { nametable1_offset, nametable2_offset };
}

bool ppu_step(PPU* self, usize cycles) {
    bool new_frame = false;

    if (self->should_trigger_nmi && (self->ctrl_reg & PPU_CTRL_NMI_ENABLE) && (self->status_reg & PPU_STATUS_VBLANK)) {
        self->should_trigger_nmi = false;
        self->nmi_triggered = true;
    }

    for (usize i = 0; i < cycles; i++) {
        if (ppu_sprite_zero_hit(self)) {
            self->status_reg |= PPU_STATUS_SPRITE0_HIT;
        }

        if (self->scanlines < 240 && self->dots == 256 && (self->mask_reg & PPU_MASK_SHOW_BACKGROUND) && (self->scanlines & 7) == 0) {
            usize bank_offset = (self->ctrl_reg & PPU_CTRL_BACKGROUND_PATTERN_TABLE) ? 0x1000 : 0;
            usize row = self->scanlines >> 3;
            ppu_render_row(self, row, self->nametable_offsets.nametable1_offset, bank_offset, -((int)self->scroll_x), self->scroll_x, SCREEN_WIDTH);
            ppu_render_row(self, row, self->nametable_offsets.nametable2_offset, bank_offset, SCREEN_WIDTH - (int)self->scroll_x, 0, self->scroll_x);
        }

        if (self->dots == 1) {
            if (self->scanlines == 241) {
                new_frame = true;
                self->frame_count++;
                self->status_reg |= PPU_STATUS_VBLANK;
                self->status_reg &= ~PPU_STATUS_SPRITE0_HIT;
                ppu_detect_nmi_edge(self);
            } else if (self->scanlines == 261) {
                self->status_reg &= ~(PPU_STATUS_VBLANK | PPU_STATUS_SPRITE0_HIT | PPU_STATUS_SPRITE_OVERFLOW);
                self->scanlines = 0;
                ppu_detect_nmi_edge(self);

                if (self->cart->reset_nametable_hack) {
                    // The status bar in Super Mario Bros flickers because of inaccurate scrolling handling
                    // this hack fixes this without requiring expensive scrolling computations
                    // see https://forums.nesdev.org/viewtopic.php?f=3&t=10762
                    self->ctrl_reg &= ~PPU_CTRL_BASE_NAMETABLE_ADDR;
                    self->nametable_offsets = ppu_get_nametable_offsets(self, self->ctrl_reg & PPU_CTRL_BASE_NAMETABLE_ADDR);
                }
            }
        }

        if (++self->dots > 340) {
            self->dots = 0;
            self->scanlines++;
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
