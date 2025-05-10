#ifndef NESC_CART_H
#define NESC_CART_H

#include "types.h"
#include "ines.h"

#define HASH_OFFSET_BASIS 2166136261
#define HASH_PRIME 16777619

typedef struct {
    INES header;
    u32 hash;
    bool reset_nametable_hack;
    u8* prg_rom;
    usize prg_size;
    u8* chr_rom;
    usize chr_size;
} Cart;

Cart cart_create(INES header, u8* prg_rom, usize prg_size, u8* chr_rom, usize chr_size);
void cart_free(Cart* cart);

#endif
