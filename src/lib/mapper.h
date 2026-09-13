#ifndef NESC_MAPPER_H
#define NESC_MAPPER_H

#include "types.h"
#include "cart.h"

typedef struct mapper Mapper;

struct mapper {
    void (*init)(Mapper* self, Cart* cart);
    void (*reset)(Mapper* self);
    void (*write)(Mapper* self, u16 addr, u8 value);
    u8 (*read)(Mapper* self, u16 addr);
    void (*ppu_address)(Mapper* self, u16 addr, usize ppu_cycle);
    void (*ppu_tick)(Mapper* self);
    bool (*is_asserting_irq)(Mapper* self);
    void (*free)(Mapper* self);
};

static inline void mapper_ppu_address(Mapper* self, u16 addr, usize ppu_cycle) {
    if (self->ppu_address != NULL) {
        self->ppu_address(self, addr, ppu_cycle);
    }
}

static inline void mapper_ppu_tick(Mapper* self) {
    if (self->ppu_tick != NULL) {
        self->ppu_tick(self);
    }
}

static inline bool mapper_is_asserting_irq(Mapper* self) {
    return self->is_asserting_irq != NULL && self->is_asserting_irq(self);
}

#endif
