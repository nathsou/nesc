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
    pc = read_word(CPU_RESET_VECTOR);

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

u8 read_byte(u16 addr) {
    if (addr < 0x2000) {
        return ram[addr & 0b0000011111111111];
    }

    if (addr < 0x4000) {
        return ppu_read_register(0x2000 + (addr & 0b111));
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
    }

    if (addr >= 0x8000) {
        return prg[addr - 0x8000];
    }

    return 0;
}

void write_byte(u16 addr, u8 value) {
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
    }
}

u16 read_word(u16 addr) {
    // little endian
    u16 low_byte = (u16)read_byte(addr);
    u16 high_byte = (u16)(((u16)read_byte(addr + 1)) << 8);
    u16 word = high_byte | low_byte;
    return word;
}

inline void write_word(u16 addr, u16 value) {
    write_byte(addr, value & 0xff);
    write_byte(addr + 1, value >> 8);
}

// addressing mode utils

inline void update_nz(u8 value) {
    zero_flag = value == 0;
    neg_flag = (value & 0b010000000) != 0;
}

inline u8 zero_page(u8 addr) {
    return read_byte(addr);
}

inline u8 zero_page_x(u8 addr) {
    return read_byte((addr + x) & 0xff); // TODO: check if wrapping is necessary
}

inline u8 zero_page_y(u8 addr) {
    return read_byte(addr + y);
}

inline u8 absolute(u16 addr) {
    return read_byte(addr);
}

inline u8 absolute_x(u16 addr) {
    return read_byte(addr + x);
}

inline u8 absolute_y(u16 addr) {
    return read_byte(addr + y);
}

inline u16 indirect_x_addr(u8 addr) {
    return read_word(addr + x);
}

inline u16 indirect_y_addr(u8 addr) {
    return read_word(addr) + y;
}

inline u8 indirect_x_val(u8 addr) {
    return read_byte(indirect_x_addr(addr));
}

inline u8 indirect_y_val(u8 addr) {
    return read_byte(indirect_y_addr(addr));
}

inline void push(u8 value) {
    write_byte((u16)(CPU_STACK_START + sp), value);
    sp--;
}

void push_word(u16 value) {
    push((u8)(value >> 8));
    push((u8)(value & 0xff));
}

inline u8 pull(void) {
    sp++;
    return read_byte((u16)(CPU_STACK_START + sp));
}

inline u16 pull_word(void) {
    u8 low_byte = pull();
    u8 high_byte = pull();
    return (u16)((high_byte << 8) | low_byte);
}

inline u8 next_byte(void) {
    u8 byte = read_byte(pc);
    pc++;
    return byte;
}

inline u16 next_word(void) {
    u16 low_byte = (u16)next_byte();
    u16 high_byte = (u16)next_byte();
    return (u16)((high_byte << 8) | low_byte);
}

u8 get_flags(void) {
    u8 flags = 0;
    flags |= carry_flag ? 1 : 0;
    flags |= zero_flag ? 2 : 0;
    flags |= neg_flag ? 4 : 0;
    flags |= (1 << 5); // unused
    flags |= overflow_flag ? 16 : 0;
    flags |= brk_flag ? 32 : 0;
    return flags;
}

void set_flags(u8 flags) {
    carry_flag = (flags & 1) != 0;
    zero_flag = (flags & 2) != 0;
    neg_flag = (flags & 4) != 0;
    overflow_flag = (flags & 16) != 0;
    brk_flag = (flags & 32) != 0;
}

void step(void) {
    u8 opcode = read_byte(pc);
    // printf("PC: %04X, opcode: %02X, A: %02X, X: %02X, Y: %02X, SP: %02X\n", pc, opcode, a, x, y, sp);
    pc++;

    switch (opcode) {
        case 0x00: brk(); break;
        case 0xEA: nop(); break;
        case 0xA9: lda_imm(next_byte()); break;
        case 0xA5: lda_zp(next_byte()); break;
        case 0xB5: lda_zpx(next_byte()); break;
        case 0xAD: lda_abs(next_word()); break;
        case 0xBD: lda_absx(next_word()); break;
        case 0xB9: lda_absy(next_word()); break;
        case 0xA1: lda_indx(next_byte()); break;
        case 0xB1: lda_indy(next_byte()); break;
        case 0xA2: ldx_imm(next_byte()); break;
        case 0xA6: ldx_zp(next_byte()); break;
        case 0xB6: ldx_zpy(next_byte()); break;
        case 0xAE: ldx_abs(next_word()); break;
        case 0xBE: ldx_absy(next_word()); break;
        case 0xA0: ldy_imm(next_byte()); break;
        case 0xA4: ldy_zp(next_byte()); break;
        case 0xB4: ldy_zpx(next_byte()); break;
        case 0xAC: ldy_abs(next_word()); break;
        case 0xBC: ldy_absx(next_word()); break;
        case 0x85: sta_zp(next_byte()); break;
        case 0x95: sta_zpx(next_byte()); break;
        case 0x8D: sta_abs(next_word()); break;
        case 0x9D: sta_absx(next_word()); break;
        case 0x99: sta_absy(next_word()); break;
        case 0x81: sta_indx(next_byte()); break;
        case 0x91: sta_indy(next_byte()); break;
        case 0x86: stx_zp(next_byte()); break;
        case 0x96: stx_zpy(next_byte()); break;
        case 0x8E: stx_abs(next_word()); break;
        case 0x84: sty_zp(next_byte()); break;
        case 0x94: sty_zpx(next_byte()); break;
        case 0x8C: sty_abs(next_word()); break;
        case 0x69: adc_imm(next_byte()); break;
        case 0x65: adc_zp(next_byte()); break;
        case 0x75: adc_zpx(next_byte()); break;
        case 0x6D: adc_abs(next_word()); break;
        case 0x7D: adc_absx(next_word()); break;
        case 0x79: adc_absy(next_word()); break;
        case 0x61: adc_indx(next_byte()); break;
        case 0x71: adc_indy(next_byte()); break;
        case 0xE9: sbc_imm(next_byte()); break;
        case 0xE5: sbc_zp(next_byte()); break;
        case 0xF5: sbc_zpx(next_byte()); break;
        case 0xED: sbc_abs(next_word()); break;
        case 0xFD: sbc_absx(next_word()); break;
        case 0xF9: sbc_absy(next_word()); break;
        case 0xE1: sbc_indx(next_byte()); break;
        case 0xF1: sbc_indy(next_byte()); break;
        case 0xAA: tax(); break;
        case 0xA8: tay(); break;
        case 0xBA: tsx(); break;
        case 0x8A: txa(); break;
        case 0x9A: txs(); break;
        case 0x98: tya(); break;
        case 0x29: and_imm(next_byte()); break;
        case 0x25: and_zp(next_byte()); break;
        case 0x35: and_zpx(next_byte()); break;
        case 0x2D: and_abs(next_word()); break;
        case 0x3D: and_absx(next_word()); break;
        case 0x39: and_absy(next_word()); break;
        case 0x21: and_indx(next_byte()); break;
        case 0x31: and_indy(next_byte()); break;
        case 0x09: ora_imm(next_byte()); break;
        case 0x05: ora_zp(next_byte()); break;
        case 0x15: ora_zpx(next_byte()); break;
        case 0x0D: ora_abs(next_word()); break;
        case 0x1D: ora_absx(next_word()); break;
        case 0x19: ora_absy(next_word()); break;
        case 0x01: ora_indx(next_byte()); break;
        case 0x11: ora_indy(next_byte()); break;
        case 0x49: eor_imm(next_byte()); break;
        case 0x45: eor_zp(next_byte()); break;
        case 0x55: eor_zpx(next_byte()); break;
        case 0x4D: eor_abs(next_word()); break;
        case 0x5D: eor_absx(next_word()); break;
        case 0x59: eor_absy(next_word()); break;
        case 0x41: eor_indx(next_byte()); break;
        case 0x51: eor_indy(next_byte()); break;
        case 0x0A: asl_acc(); break;
        case 0x06: asl_zp(next_byte()); break;
        case 0x16: asl_zpx(next_byte()); break;
        case 0x0E: asl_abs(next_word()); break;
        case 0x1E: asl_absx(next_word()); break;
        case 0x4A: lsr_acc(); break;
        case 0x46: lsr_zp(next_byte()); break;
        case 0x56: lsr_zpx(next_byte()); break;
        case 0x4E: lsr_abs(next_word()); break;
        case 0x5E: lsr_absx(next_word()); break;
        case 0xE6: inc_zp(next_byte()); break;
        case 0xF6: inc_zpx(next_byte()); break;
        case 0xEE: inc_abs(next_word()); break;
        case 0xFE: inc_absx(next_word()); break;
        case 0xE8: inx(); break;
        case 0xC8: iny(); break;
        case 0xC6: dec_zp(next_byte()); break;
        case 0xD6: dec_zpx(next_byte()); break;
        case 0xCE: dec_abs(next_word()); break;
        case 0xDE: dec_absx(next_word()); break;
        case 0xCA: dex(); break;
        case 0x88: dey(); break;
        case 0x4C: jmp_abs(next_word()); break;
        case 0x6C: jmp_ind(next_word()); break;
        case 0x90: bcc_rel(next_byte()); break;
        case 0xB0: bcs_rel(next_byte()); break;
        case 0xF0: beq_rel(next_byte()); break;
        case 0xD0: bne_rel(next_byte()); break;
        case 0x10: bpl_rel(next_byte()); break;
        case 0x30: bmi_rel(next_byte()); break;
        case 0x50: bvc_rel(next_byte()); break;
        case 0x70: bvs_rel(next_byte()); break;
        case 0x18: clc(); break;
        case 0x38: sec(); break;
        case 0xD8: cld(); break;
        case 0xF8: sed(); break;
        case 0x58: cli(); break;
        case 0x78: sei(); break;
        case 0xB8: clv(); break;
        case 0xC9: cmp_imm(next_byte()); break;
        case 0xC5: cmp_zp(next_byte()); break;
        case 0xD5: cmp_zpx(next_byte()); break;
        case 0xCD: cmp_abs(next_word()); break;
        case 0xDD: cmp_absx(next_word()); break;
        case 0xD9: cmp_absy(next_word()); break;
        case 0xC1: cmp_indx(next_byte()); break;
        case 0xD1: cmp_indy(next_byte()); break;
        case 0xE0: cpx_imm(next_byte()); break;
        case 0xE4: cpx_zp(next_byte()); break;
        case 0xEC: cpx_abs(next_word()); break;
        case 0xC0: cpy_imm(next_byte()); break;
        case 0xC4: cpy_zp(next_byte()); break;
        case 0xCC: cpy_abs(next_word()); break;
        case 0x48: pha(); break;
        case 0x68: pla(); break;
        case 0x28: plp(); break;
        case 0x08: php(); break;
        case 0x20: jsr(); break;
        case 0x60: rts(); break;
        case 0x40: rti(); break;
        case 0x24: bit_zp(next_byte()); break;
        case 0x2C: bit_abs(next_word()); break;
        case 0x2A: rol_acc(); break;
        case 0x26: rol_zp(next_byte()); break;
        case 0x36: rol_zpx(next_byte()); break;
        case 0x2E: rol_abs(next_word()); break;
        case 0x3E: rol_absx(next_word()); break;
        case 0x6A: ror_acc(); break;
        case 0x66: ror_zp(next_byte()); break;
        case 0x76: ror_zpx(next_byte()); break;
        case 0x6E: ror_abs(next_word()); break;
        case 0x7E: ror_absx(next_word()); break;
        default:
            printf("Unsupported instruction: 0x%02X at PC: 0x%04X\n", opcode, pc - 1);
            exit(1); 
            break;
    }

    cycles += INST_CYCLES[opcode];
}

void cpu_step_frame(void) {
    cycles = 0;

    while (cycles < CPU_CYCLES_PER_FRAME) {
        step();
    }
}
