#ifndef NESC_MAPPER_H
#define NESC_MAPPER_H

#include "types.h"
#include "cart.h"

typedef struct mapper Mapper;

struct mapper {
    void (*init)(Mapper* self, Cart cart);
    void (*reset)(Mapper* self);
    void (*write)(Mapper* self, u16 addr, u8 value);
    u8 (*read)(Mapper* self, u16 addr);
    void (*free)(Mapper* self);
};

#endif
