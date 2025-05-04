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
} CartMetadata;

CartMetadata cart_create(INES header, const u8* prg_rom, usize prg_size);

#endif
