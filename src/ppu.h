#ifndef SMB_PPU_H
#define SMB_PPU_H

#include "types.h"
#include <string.h>
#include <stdlib.h>

#define SCREEN_WIDTH 256
#define SCREEN_HEIGHT 240

#define PPU_CTRL_NMI_ENABLE 128
#define PPU_CTRL_SPRITE_SIZE 64
#define PPU_CTRL_SPRITE_PATTERN_TABLE 32
#define PPU_CTRL_BACKGROUND_PATTERN_TABLE 16
#define PPU_CTRL_VRAM_INCREMENT 8
#define PPU_CTRL_BASE_NAMETABLE_ADDR 0b11

#define PPU_MASK_SHOW_BACKGROUND_LEFTMOST 2
#define PPU_MASK_SHOW_SPRITES_LEFTMOST 4
#define PPU_MASK_SHOW_BACKGROUND 8
#define PPU_MASK_SHOW_SPRITES 16

#define PPU_STATUS_SPRITE_OVERFLOW 32
#define PPU_STATUS_SPRITE0_HIT 64
#define PPU_STATUS_VBLANK 128

extern u8* chr_rom; // 1 page of CHR ROM (8KB)
extern u8 nametable[2048]; // 2KB of nametable RAM
extern u8 palette_table[32]; // 32 bytes of palette RAM
extern u8 oam[256]; // 256 bytes of OAM RAM

extern u16 ppu_v; // current VRAM address
extern u8 ppu_w; // write toggle (1 bit)
extern u8 ppu_f; // even/odd frame flag (1 bit)

// PPU registers
extern u8 ppu_ctrl; // Control register: $2000
extern u8 ppu_mask; // Mask register: $2001
extern u8 ppu_status; // Status register: $2002
extern u8 oam_addr; // OAM address: $2003
extern u8 ppu_scroll_x;
extern u8 ppu_scroll_y;

extern u16 vram_addr;
extern u8 vram_internal_buffer; // VRAM read/write buffer
extern u8 oam_dma; // OAM DMA: $4014

void ppu_transfer_oam(u16 start_addr);

// screen
extern u8 frame[SCREEN_WIDTH * SCREEN_HEIGHT * 3]; // 3 bytes per pixel (RGB)

void ppu_init(u8* chr);
void ppu_free(void);

u8 ppu_read_register(u16 addr);
void ppu_write_register(u16 addr, u8 value);

u8 ppu_read(u16 addr);
void ppu_write(u16 addr, u8 value);
bool ppu_step(usize cycles);
void ppu_render(void);

#endif
