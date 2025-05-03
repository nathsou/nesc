#include "cpu.h"
#include "ppu.h"
#include "apu.h"
#include <stdio.h> // for printf
#include <stdlib.h> // for exit

u8 a;
u8 x;
u8 y;
u8 sp;
u16 pc;

bool carry_flag;
bool zero_flag;
bool neg_flag;
bool overflow_flag;
bool brk_flag;
bool interrupt_disable_flag;

u8* prg;
usize prog_rom_size;
u8 ram[2048];
u8 prg_ram[2 * 1024];

u8 controller1_state;
bool controller1_strobe;
u8 controller1_btn_index;
usize cycles;

usize INST_CYCLES[] = {
    7, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 4, 4, 6, 6, 2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 4, 4, 6, 6, 2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    6, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 3, 4, 6, 6, 2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 5, 4, 6, 6, 2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4, 2, 6, 2, 6, 4, 4, 4, 4, 2, 5, 2, 5, 5, 5, 5, 5,
    2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4, 2, 5, 2, 5, 4, 4, 4, 4, 2, 4, 2, 4, 4, 4, 4, 4,
    2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6, 2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6, 2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
};

inline void update_controller1(u8 state) {
    controller1_state = state;
}

void cpu_init(u8 *prg_rom, usize size) {
    prg = prg_rom;
    prog_rom_size = size;

    // registers
    a = 0;
    x = 0;
    y = 0;
    sp = CPU_STACK_TOP;
    pc = cpu_read_word(CPU_RESET_VECTOR);

    // flags
    carry_flag = false;
    zero_flag = false;
    neg_flag = false;
    overflow_flag = false;
    brk_flag = false;
    interrupt_disable_flag = false;

    // controller
    controller1_state = 0;
    controller1_strobe = false;
    controller1_btn_index = 0;

    cycles = 0;
}

void cpu_free(void) {
    free(prg);
}

u8 cpu_read_byte(u16 addr) {
    if (addr < 0x2000) {
        return ram[addr & 0x7ff];
    }

    if (addr < 0x4000) {
        return ppu_read_register(0x2000 + (addr & 7));
    }

    if (addr == 0x4016) {
        if (controller1_btn_index > 7) {
            return 1;
        }

        u8 state = (controller1_state & (1 << controller1_btn_index)) >> controller1_btn_index;

        if (!controller1_strobe && controller1_btn_index < 8) {
            controller1_btn_index++;
        }
        
        return state;
    }

    if (addr < 0x4020) {
        // APU
        return 0;
    }

    if (addr >= 0x6000 && addr < 0x8000) {
        // PRG RAM
        return prg_ram[((addr - 0x6000) & 0x7FF)];
    }

    if (addr >= 0x8000 && addr <= 0xBFFF) {
        // NROM: first 16KB of PRG ROM
        return prg[addr - 0x8000];
    }

    if (addr >= 0x8000) {
        u16 prg_rom_addr = addr - 0x8000;

        if (prog_rom_size == 16 * 1024 && prg_rom_addr >= 0x4000) {
            // last 16KB of PRG ROM
            return prg[prg_rom_addr - 0x4000];
        } else {
            // NROM: first 16KB of PRG ROM
            return prg[prg_rom_addr];
        }
    }

    printf("cpu_read_byte: %04x out of range\n", addr);
    // exit(1);
    return 0;
}

void cpu_write_byte(u16 addr, u8 value) {
    if (addr < 0x2000) {
        ram[addr & 0b0000011111111111] = value;
    } else if (addr < 0x4000) {
        ppu_write_register(0x2000 + (addr & 0b111), value);
    } else if (addr == 0x4014) {
        u16 start_addr = (u16)(value << 8);
        ppu_transfer_oam(start_addr);
    } else if (addr == 0x4016) {
        // controller 1
        controller1_strobe = (value & 1) == 1;
        
        if (controller1_strobe) {
            controller1_btn_index = 0;
        }
    } else if (addr < 0x4020) {
        apu_write(addr, value);
    } else if (addr >= 0x6000 && addr < 0x8000) {
        // PRG RAM
        prg_ram[((addr - 0x6000) & 0x7FF)] = value;
    } else if (addr < 0xFFFF) {
        // PRG ROM
        if (prog_rom_size == 16 * 1024) {
            // mirror first 16KB of PRG ROM
            prg[(addr - 0x8000) & 0x3FFF] = value;
        } else {
            // NROM: last 16KB of PRG ROM
            prg[addr - 0x8000] = value;
        }
    }
}

u16 cpu_read_word(u16 addr) {
    // little endian
    u16 low_byte = (u16)cpu_read_byte(addr);
    u16 high_byte = (u16)(((u16)cpu_read_byte(addr + 1)) << 8);
    u16 word = high_byte | low_byte;
    return word;
}

inline void cpu_write_word(u16 addr, u16 value) {
    cpu_write_byte(addr, value & 0xff);
    cpu_write_byte(addr + 1, value >> 8);
}

// addressing mode utils

inline void cpu_update_nz(u8 value) {
    zero_flag = value == 0;
    neg_flag = (value & 0b010000000) != 0;
}

inline u8 zero_page(u8 addr) {
    return cpu_read_byte(addr);
}

inline u8 zero_page_x(u8 addr) {
    return cpu_read_byte((addr + x) & 0xff); // TODO: check if wrapping is necessary
}

inline u8 zero_page_y(u8 addr) {
    return cpu_read_byte(addr + y);
}

inline u8 absolute(u16 addr) {
    return cpu_read_byte(addr);
}

inline u8 absolute_x(u16 addr) {
    return cpu_read_byte(addr + x);
}

inline u8 absolute_y(u16 addr) {
    return cpu_read_byte(addr + y);
}

inline u16 indirect_x_addr(u8 addr) {
    return cpu_read_word(addr + x);
}

inline u16 indirect_y_addr(u8 addr) {
    return cpu_read_word(addr) + y;
}

inline u8 indirect_x_val(u8 addr) {
    return cpu_read_byte(indirect_x_addr(addr));
}

inline u8 indirect_y_val(u8 addr) {
    return cpu_read_byte(indirect_y_addr(addr));
}

inline void cpu_push(u8 value) {
    cpu_write_byte((u16)(CPU_STACK_START + sp), value);
    sp--;
}

void cpu_push_word(u16 value) {
    cpu_push((u8)(value >> 8));
    cpu_push((u8)(value & 0xff));
}

inline u8 cpu_pull(void) {
    sp++;
    return cpu_read_byte((u16)(CPU_STACK_START + sp));
}

inline u16 cpu_pull_word(void) {
    u8 low_byte = cpu_pull();
    u8 high_byte = cpu_pull();
    return (u16)((high_byte << 8) | low_byte);
}

inline u8 cpu_next_byte(void) {
    u8 byte = cpu_read_byte(pc);
    pc++;
    return byte;
}

inline u16 cpu_next_word(void) {
    u16 low_byte = (u16)cpu_next_byte();
    u16 high_byte = (u16)cpu_next_byte();
    return (u16)((high_byte << 8) | low_byte);
}

u8 cpu_get_flags(void) {
    u8 flags = 0;
    flags |= carry_flag ? 1 : 0;
    flags |= zero_flag ? 2 : 0;
    flags |= neg_flag ? 4 : 0;
    flags |= (1 << 5); // unused
    flags |= overflow_flag ? 16 : 0;
    flags |= brk_flag ? 32 : 0;
    return flags;
}

void cpu_set_flags(u8 flags) {
    carry_flag = (flags & 1) != 0;
    zero_flag = (flags & 2) != 0;
    neg_flag = (flags & 4) != 0;
    overflow_flag = (flags & 16) != 0;
    brk_flag = (flags & 32) != 0;
}

usize cpu_step(void) {
    u8 opcode = cpu_read_byte(pc);
    // printf("PC: %04X, opcode: %02X, A: %02X, X: %02X, Y: %02X, SP: %02X\n", pc, opcode, a, x, y, sp);
    pc++;

    switch (opcode) {
        case 0x00: brk(); break;
        case 0xEA: nop(); break;
        case 0xA9: lda_imm(cpu_next_byte()); break;
        case 0xA5: lda_zp(cpu_next_byte()); break;
        case 0xB5: lda_zpx(cpu_next_byte()); break;
        case 0xAD: lda_abs(cpu_next_word()); break;
        case 0xBD: lda_absx(cpu_next_word()); break;
        case 0xB9: lda_absy(cpu_next_word()); break;
        case 0xA1: lda_indx(cpu_next_byte()); break;
        case 0xB1: lda_indy(cpu_next_byte()); break;
        case 0xA2: ldx_imm(cpu_next_byte()); break;
        case 0xA6: ldx_zp(cpu_next_byte()); break;
        case 0xB6: ldx_zpy(cpu_next_byte()); break;
        case 0xAE: ldx_abs(cpu_next_word()); break;
        case 0xBE: ldx_absy(cpu_next_word()); break;
        case 0xA0: ldy_imm(cpu_next_byte()); break;
        case 0xA4: ldy_zp(cpu_next_byte()); break;
        case 0xB4: ldy_zpx(cpu_next_byte()); break;
        case 0xAC: ldy_abs(cpu_next_word()); break;
        case 0xBC: ldy_absx(cpu_next_word()); break;
        case 0x85: sta_zp(cpu_next_byte()); break;
        case 0x95: sta_zpx(cpu_next_byte()); break;
        case 0x8D: sta_abs(cpu_next_word()); break;
        case 0x9D: sta_absx(cpu_next_word()); break;
        case 0x99: sta_absy(cpu_next_word()); break;
        case 0x81: sta_indx(cpu_next_byte()); break;
        case 0x91: sta_indy(cpu_next_byte()); break;
        case 0x86: stx_zp(cpu_next_byte()); break;
        case 0x96: stx_zpy(cpu_next_byte()); break;
        case 0x8E: stx_abs(cpu_next_word()); break;
        case 0x84: sty_zp(cpu_next_byte()); break;
        case 0x94: sty_zpx(cpu_next_byte()); break;
        case 0x8C: sty_abs(cpu_next_word()); break;
        case 0x69: adc_imm(cpu_next_byte()); break;
        case 0x65: adc_zp(cpu_next_byte()); break;
        case 0x75: adc_zpx(cpu_next_byte()); break;
        case 0x6D: adc_abs(cpu_next_word()); break;
        case 0x7D: adc_absx(cpu_next_word()); break;
        case 0x79: adc_absy(cpu_next_word()); break;
        case 0x61: adc_indx(cpu_next_byte()); break;
        case 0x71: adc_indy(cpu_next_byte()); break;
        case 0xE9: sbc_imm(cpu_next_byte()); break;
        case 0xE5: sbc_zp(cpu_next_byte()); break;
        case 0xF5: sbc_zpx(cpu_next_byte()); break;
        case 0xED: sbc_abs(cpu_next_word()); break;
        case 0xFD: sbc_absx(cpu_next_word()); break;
        case 0xF9: sbc_absy(cpu_next_word()); break;
        case 0xE1: sbc_indx(cpu_next_byte()); break;
        case 0xF1: sbc_indy(cpu_next_byte()); break;
        case 0xAA: tax(); break;
        case 0xA8: tay(); break;
        case 0xBA: tsx(); break;
        case 0x8A: txa(); break;
        case 0x9A: txs(); break;
        case 0x98: tya(); break;
        case 0x29: and_imm(cpu_next_byte()); break;
        case 0x25: and_zp(cpu_next_byte()); break;
        case 0x35: and_zpx(cpu_next_byte()); break;
        case 0x2D: and_abs(cpu_next_word()); break;
        case 0x3D: and_absx(cpu_next_word()); break;
        case 0x39: and_absy(cpu_next_word()); break;
        case 0x21: and_indx(cpu_next_byte()); break;
        case 0x31: and_indy(cpu_next_byte()); break;
        case 0x09: ora_imm(cpu_next_byte()); break;
        case 0x05: ora_zp(cpu_next_byte()); break;
        case 0x15: ora_zpx(cpu_next_byte()); break;
        case 0x0D: ora_abs(cpu_next_word()); break;
        case 0x1D: ora_absx(cpu_next_word()); break;
        case 0x19: ora_absy(cpu_next_word()); break;
        case 0x01: ora_indx(cpu_next_byte()); break;
        case 0x11: ora_indy(cpu_next_byte()); break;
        case 0x49: eor_imm(cpu_next_byte()); break;
        case 0x45: eor_zp(cpu_next_byte()); break;
        case 0x55: eor_zpx(cpu_next_byte()); break;
        case 0x4D: eor_abs(cpu_next_word()); break;
        case 0x5D: eor_absx(cpu_next_word()); break;
        case 0x59: eor_absy(cpu_next_word()); break;
        case 0x41: eor_indx(cpu_next_byte()); break;
        case 0x51: eor_indy(cpu_next_byte()); break;
        case 0x0A: asl_acc(); break;
        case 0x06: asl_zp(cpu_next_byte()); break;
        case 0x16: asl_zpx(cpu_next_byte()); break;
        case 0x0E: asl_abs(cpu_next_word()); break;
        case 0x1E: asl_absx(cpu_next_word()); break;
        case 0x4A: lsr_acc(); break;
        case 0x46: lsr_zp(cpu_next_byte()); break;
        case 0x56: lsr_zpx(cpu_next_byte()); break;
        case 0x4E: lsr_abs(cpu_next_word()); break;
        case 0x5E: lsr_absx(cpu_next_word()); break;
        case 0xE6: inc_zp(cpu_next_byte()); break;
        case 0xF6: inc_zpx(cpu_next_byte()); break;
        case 0xEE: inc_abs(cpu_next_word()); break;
        case 0xFE: inc_absx(cpu_next_word()); break;
        case 0xE8: inx(); break;
        case 0xC8: iny(); break;
        case 0xC6: dec_zp(cpu_next_byte()); break;
        case 0xD6: dec_zpx(cpu_next_byte()); break;
        case 0xCE: dec_abs(cpu_next_word()); break;
        case 0xDE: dec_absx(cpu_next_word()); break;
        case 0xCA: dex(); break;
        case 0x88: dey(); break;
        case 0x4C: jmp_abs(cpu_next_word()); break;
        case 0x6C: jmp_ind(cpu_next_word()); break;
        case 0x90: bcc_rel(cpu_next_byte()); break;
        case 0xB0: bcs_rel(cpu_next_byte()); break;
        case 0xF0: beq_rel(cpu_next_byte()); break;
        case 0xD0: bne_rel(cpu_next_byte()); break;
        case 0x10: bpl_rel(cpu_next_byte()); break;
        case 0x30: bmi_rel(cpu_next_byte()); break;
        case 0x50: bvc_rel(cpu_next_byte()); break;
        case 0x70: bvs_rel(cpu_next_byte()); break;
        case 0x18: clc(); break;
        case 0x38: sec(); break;
        case 0xD8: cld(); break;
        case 0xF8: sed(); break;
        case 0x58: cli(); break;
        case 0x78: sei(); break;
        case 0xB8: clv(); break;
        case 0xC9: cmp_imm(cpu_next_byte()); break;
        case 0xC5: cmp_zp(cpu_next_byte()); break;
        case 0xD5: cmp_zpx(cpu_next_byte()); break;
        case 0xCD: cmp_abs(cpu_next_word()); break;
        case 0xDD: cmp_absx(cpu_next_word()); break;
        case 0xD9: cmp_absy(cpu_next_word()); break;
        case 0xC1: cmp_indx(cpu_next_byte()); break;
        case 0xD1: cmp_indy(cpu_next_byte()); break;
        case 0xE0: cpx_imm(cpu_next_byte()); break;
        case 0xE4: cpx_zp(cpu_next_byte()); break;
        case 0xEC: cpx_abs(cpu_next_word()); break;
        case 0xC0: cpy_imm(cpu_next_byte()); break;
        case 0xC4: cpy_zp(cpu_next_byte()); break;
        case 0xCC: cpy_abs(cpu_next_word()); break;
        case 0x48: pha(); break;
        case 0x68: pla(); break;
        case 0x28: plp(); break;
        case 0x08: php(); break;
        case 0x20: jsr(); break;
        case 0x60: rts(); break;
        case 0x40: rti(); break;
        case 0x24: bit_zp(cpu_next_byte()); break;
        case 0x2C: bit_abs(cpu_next_word()); break;
        case 0x2A: rol_acc(); break;
        case 0x26: rol_zp(cpu_next_byte()); break;
        case 0x36: rol_zpx(cpu_next_byte()); break;
        case 0x2E: rol_abs(cpu_next_word()); break;
        case 0x3E: rol_absx(cpu_next_word()); break;
        case 0x6A: ror_acc(); break;
        case 0x66: ror_zp(cpu_next_byte()); break;
        case 0x76: ror_zpx(cpu_next_byte()); break;
        case 0x6E: ror_abs(cpu_next_word()); break;
        case 0x7E: ror_absx(cpu_next_word()); break;
        default:
            printf("Unsupported instruction: 0x%02X at PC: 0x%04X\n", opcode, pc - 1);
            exit(1); 
            break;
    }

    cycles += INST_CYCLES[opcode];
    usize step_cycles = cycles;
    cycles = 0;

    return step_cycles;
}

