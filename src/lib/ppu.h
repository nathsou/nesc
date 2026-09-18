#ifndef NESC_PPU_H
#define NESC_PPU_H

#include "types.h"
#include "cart.h"
#include "mapper.h"
#include <string.h>
#include <stdlib.h>

#define SCREEN_WIDTH 256
#define SCREEN_HEIGHT 240
#define PPU_FRAME_RGBA 1

#define PPU_CTRL_NMI_ENABLE 128
#define PPU_CTRL_SPRITE_SIZE 32
#define PPU_CTRL_BACKGROUND_PATTERN_TABLE 16
#define PPU_CTRL_SPRITE_PATTERN_TABLE 8
#define PPU_CTRL_VRAM_INCREMENT 4
#define PPU_CTRL_BASE_NAMETABLE_ADDR 0b11

#define PPU_MASK_SHOW_BACKGROUND_LEFTMOST 2
#define PPU_MASK_SHOW_SPRITES_LEFTMOST 4
#define PPU_MASK_SHOW_BACKGROUND 8
#define PPU_MASK_SHOW_SPRITES 16

#define PPU_STATUS_SPRITE_OVERFLOW 32
#define PPU_STATUS_SPRITE0_HIT 64
#define PPU_STATUS_VBLANK 128

#define PPU_V_COARSE_X_SCROLL  0b000000000011111
#define PPU_V_COARSE_Y_SCROLL  0b000001111100000
#define PPU_V_NAMETABLE_SELECT 0b000110000000000
#define PPU_V_FINE_Y_SCROLL    0b111000000000000

typedef struct {
    u16 x;
    u8 tile_index;
    u8 chr[8];
    u8 palette_index;
    bool behind_background;
} SpriteData;

typedef struct {
    u8 palette_index; // 0 means transparent; sprite palette RAM indices are nonzero
    u8 flags;
} SpriteLinePixel;

typedef struct {
    usize scanlines;
    usize dots;
    usize frame_count;
    usize total_cycles;
    Cart* cart;
    Mapper* mapper;
    u8 nametable[4096]; // four-screen cartridges require 4KB
    u8 palette_table[32]; // 32 bytes of palette RAM
    u8 oam[256]; // 256 bytes of OAM RAM
    u8 ctrl_reg;
    u8 mask_reg;
    u8 status_reg;
    u8 oam_addr_reg;
    u8 scroll_x;
    u8 scroll_y;
    u16 v_reg;
    u16 t_reg;
    u8 x_reg; // fine X scroll
    u8 data_buffer; // internal buffer for data reads
    u32 frame[SCREEN_WIDTH * SCREEN_HEIGHT];
    bool write_toggle; // w
    u8 oam_dma;
    bool nmi_triggered;
    bool nmi_edge_detector;
    bool should_trigger_nmi;
    u8 nametable_byte;
    u8 attribute_byte;
    u8 pattern_low_byte;
    u8 pattern_high_byte;
    u64 background_pixels;
    SpriteData scanline_sprites[8];
    SpriteLinePixel sprite_line[SCREEN_WIDTH];
    u8 visible_scanline_sprites;
} PPU;

void ppu_init(PPU* self, Cart* cart, Mapper* mapper);
void ppu_free(PPU* self);

u8 ppu_read_register(PPU* self, u16 addr);
void ppu_write_register(PPU* self, u16 addr, u8 value);

u8 ppu_read(PPU* self, u16 addr);
void ppu_write(PPU *self, u16 addr, u8 value);
bool ppu_step(PPU* self, usize cycles);

#endif
