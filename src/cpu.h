#ifndef SMB_CPU_H
#define SMB_CPU_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <memory.h>
#include "types.h"
#include "instructions.h"
#include <stdlib.h>

#define CPU_NMI_VECTOR 0xFFFA
#define CPU_RESET_VECTOR 0xFFFC
#define CPU_IRQ_VECTOR 0xFFFE
#define CPU_STACK_TOP 0xFD
#define CPU_STACK_START 0x100
#define CPU_FREQ 1789773 // 1.79 MHz
#define CPU_CYCLES_PER_FRAME (CPU_FREQ / 60)

// registers
extern u8 a, x, y, sp;
extern u16 pc;

// flags
extern bool carry_flag, zero_flag, neg_flag, overflow_flag, brk_flag, interrupt_disable_flag;

// memory
extern u8 ram[0x800]; // 2KB

extern usize cycles;

u8 cpu_read_byte(u16 addr);
void cpu_write_byte(u16 addr, u8 value);

u16 cpu_read_word(u16 addr);
void cpu_write_word(u16 addr, u16 value);

u8 cpu_next_byte(void);
u16 cpu_next_word(void);

// controllers
extern u8 controller1_state;
void update_controller1(u8 state);

void cpu_init(u8 *prg_rom, usize prog_rom_size);
void cpu_free(void);

// addressing mode utils

void cpu_update_nz(u8 value);

u8 zero_page(u8 addr);
u8 zero_page_x(u8 addr);
u8 zero_page_y(u8 addr);

u8 absolute(u16 addr);
u8 absolute_x(u16 addr);
u8 absolute_y(u16 addr);

u16 indirect_x_addr(u8 addr);
u16 indirect_y_addr(u8 addr);

u8 indirect_x_val(u8 addr);
u8 indirect_y_val(u8 addr);

void cpu_push(u8 value);
u8 cpu_pull(void);
u16 cpu_pull_word(void);
void cpu_push_word(u16 value);
u8 cpu_get_flags(void);
void cpu_set_flags(u8 flags);
usize cpu_step(void);

#endif
