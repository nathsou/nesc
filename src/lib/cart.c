#include "cart.h"
#include <stdlib.h>

Cart cart_create(INES header, u8* prg_rom, usize prg_size, u8* chr_rom, usize chr_size) {
    Cart cart = {
        .header = header,
        .prg_rom = prg_rom,
        .prg_size = prg_size,
        .chr_rom = chr_rom,
        .chr_size = chr_size,
    };

    return cart;
}

void cart_free(Cart *cart) {
    free(cart->prg_rom);
    free(cart->chr_rom);
    cart->prg_rom = NULL;
    cart->chr_rom = NULL;
    cart->prg_size = 0;
    cart->chr_size = 0;
}
