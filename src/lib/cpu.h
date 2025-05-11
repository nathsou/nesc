#ifndef NESC_CPU_H
#define NESC_CPU_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <memory.h>
#include "ppu.h"
#include "apu.h"
#include "mapper.h"
#include "types.h"
#include <stdlib.h>

#define CPU_NMI_VECTOR 0xFFFA
#define CPU_RESET_VECTOR 0xFFFC
#define CPU_IRQ_VECTOR 0xFFFE
#define CPU_STACK_TOP 0xFD
#define CPU_STACK_START 0x100
#define CPU_FREQ 1789773 // 1.79 MHz
#define CPU_CYCLES_PER_FRAME (CPU_FREQ / 60)

typedef struct {
    u16 pc;
    u8 a, x, y, sp;
    PPU* ppu;
    APU* apu;
    Mapper* mapper;
    bool carry_flag, zero_flag, neg_flag, decimal_flag, overflow_flag, break_flag, interrupt_disable_flag;
    u8 ram[2048];
    usize inst_cycles, total_cycles, stall_cycles;
    u8 controller1_state, controller1_btn_index;
    bool controller1_strobe;
} CPU;

u8 cpu_read_byte(CPU* self, u16 addr);
void cpu_write_byte(CPU* self, u16 addr, u8 value);

u16 cpu_read_word(CPU* self, u16 addr);
void cpu_write_word(CPU* self, u16 addr, u16 value);

// controllers
extern u8 controller1_state;
void cpu_update_controller1(CPU* self, u8 state);

void cpu_init(CPU* self, PPU* ppu, APU* apu, Mapper* mapper);
void cpu_free(CPU* self);
usize cpu_step(CPU* cpu);

#endif