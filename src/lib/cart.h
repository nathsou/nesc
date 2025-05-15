#ifndef NESC_CART_H
#define NESC_CART_H

#include "types.h"
#include "ines.h"

typedef struct {
    INES header;
    u8* prg_rom;
    usize prg_size;
    u8* chr_rom;
    usize chr_size;
} Cart;

Cart cart_create(INES header, u8* prg_rom, usize prg_size, u8* chr_rom, usize chr_size);
void cart_free(Cart* cart);

#endif
