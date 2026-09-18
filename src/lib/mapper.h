#ifndef NESC_MAPPER_H
#define NESC_MAPPER_H

#include "types.h"
#include "cart.h"

typedef struct mapper Mapper;

struct mapper {
    // Optional direct 1 KiB CHR read pages. Refresh on mapping changes.
    // PPU bus notifications and all writes still go through mapper callbacks.
    const u8* chr_pages[8];
    void (*init)(Mapper* self, Cart* cart);
    void (*reset)(Mapper* self);
    void (*write)(Mapper* self, u16 addr, u8 value);
    void (*write_rmw)(Mapper* self, u16 addr, u8 old_value, u8 new_value);
    u8 (*read)(Mapper* self, u16 addr);
    void (*ppu_address)(Mapper* self, u16 addr, usize ppu_cycle);
    void (*ppu_tick)(Mapper* self);
    void (*ppu_advance)(Mapper* self, usize cycles);
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

static inline void mapper_ppu_advance(Mapper* self, usize cycles) {
    if (self->ppu_advance != NULL) {
        self->ppu_advance(self, cycles);
    } else if (self->ppu_tick != NULL) {
        for (usize i = 0; i < cycles; i++) {
            self->ppu_tick(self);
        }
    }
}

static inline bool mapper_is_asserting_irq(Mapper* self) {
    return self->is_asserting_irq != NULL && self->is_asserting_irq(self);
}

#endif
