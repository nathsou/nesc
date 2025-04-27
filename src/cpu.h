#ifndef SMB_CPU_H
#define SMB_CPU_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <memory.h>
#include "types.h"
#include "instructions.h"

#define CPU_NMI_VECTOR 0xFFFA
#define CPU_RESET_VECTOR 0xFFFC
#define CPU_IRQ_VECTOR 0xFFFE
#define CPU_STACK_TOP 0xFD
#define CPU_STACK_START 0x100
#define CPU_FREQ 1789773 // 1.79 MHz
#define CPU_CYCLES_PER_FRAME (CPU_FREQ / 60)

extern u8 prg_rom[32 * 1024]; // 32KB

// registers
extern u8 a, x, y, sp;
extern u16 pc;

// flags
extern bool carry_flag, zero_flag, neg_flag, overflow_flag, brk_flag, interrupt_disable_flag;

// memory
extern u8 ram[0x800]; // 2KB

extern usize cycles;

u8 read_byte(u16 addr);
void write_byte(u16 addr, u8 value);

u16 read_word(u16 addr);
void write_word(u16 addr, u16 value);

u8 next_byte(void);
u16 next_word(void);

// controllers
extern u8 controller1_state;
void update_controller1(u8 state);

void cpu_init(u8 *prg_rom);

// addressing mode utils

void update_nz(u8 value);

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

void push(u8 value);
u8 pull(void);
u16 pull_word(void);
void push_word(u16 value);
u8 get_flags(void);
void set_flags(u8 flags);

// engine
void cpu_step_frame(void);

#endif
