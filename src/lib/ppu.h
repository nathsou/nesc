#ifndef NESC_PPU_H
#define NESC_PPU_H

#include "types.h"
#include "cart.h"
#include "mapper.h"
#include <string.h>
#include <stdlib.h>

#define SCREEN_WIDTH 256
#define SCREEN_HEIGHT 240

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

typedef struct {
    usize nametable1_offset;
    usize nametable2_offset;
} NametableOffsets;

typedef struct {
    usize scanlines;
    usize dots;
    usize frame_count;
    Cart* cart;
    Mapper* mapper;    
    u8 nametable[2048]; // 2KB of nametable RAM
    u8 palette_table[32]; // 32 bytes of palette RAM
    u8 oam[256]; // 256 bytes of OAM RAM
    u8 ctrl_reg;
    u8 mask_reg;
    u8 status_reg;
    u8 oam_addr_reg;
    u8 scroll_x;
    u8 scroll_y;
    u8 frame[SCREEN_WIDTH * SCREEN_HEIGHT * 3]; // 3 bytes per pixel (RGB)
    bool opaque_bg_mask[SCREEN_WIDTH * SCREEN_HEIGHT];
    u16 vram_addr; // v register
    u8 vram_internal_buffer; // VRAM read/write buffer
    bool write_toggle;
    u8 oam_dma;
    NametableOffsets nametable_offsets;
    bool nmi_triggered;
    bool nmi_edge_detector;
    bool should_trigger_nmi;
} PPU;

void ppu_init(PPU* self, Cart* cart, Mapper* mapper);
void ppu_free(PPU* self);

u8 ppu_read_register(PPU* self, u16 addr);
void ppu_write_register(PPU* self, u16 addr, u8 value);

u8 ppu_read(PPU* self, u16 addr);
void ppu_write(PPU *self, u16 addr, u8 value);
bool ppu_step(PPU* self, usize cycles);
void ppu_render(PPU* self);

#endif
