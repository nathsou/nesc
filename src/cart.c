#include "cart.h"

u32 compute_hash(const u8* data, usize size) {
    u32 hash = HASH_OFFSET_BASIS;

    for (usize i = 0; i < size; i++) {
        hash ^= data[i];
        hash *= HASH_PRIME;
    }

    return hash;
}

#define SMB_HASH 0x30AB8F8A

CartMetadata cart_create(INES header, const u8* prg_rom, usize prg_size) {
    u32 hash = compute_hash(prg_rom, prg_size);

    CartMetadata metadata = {
        .hash = hash,
        .header = header,
        .reset_nametable_hack = false,
    };

    switch (hash) {
        case SMB_HASH:
            metadata.reset_nametable_hack = true;
            break;
    }

    return metadata;
}
