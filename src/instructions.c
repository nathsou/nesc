#include "instructions.h"

// LDA - Load Accumulator

inline void lda_imm(u8 value) {
    a = value;
    cpu_update_nz(a);
}

inline void lda_zp(u8 addr) {
    lda_imm(zero_page(addr));
}

inline void lda_zpx(u8 addr) {
    lda_imm(zero_page_x(addr));
}

inline void lda_zpy(u8 addr) {
    lda_imm(zero_page_y(addr));
}

inline void lda_abs(u16 addr) {
    lda_imm(absolute(addr));
}

inline void lda_absx(u16 addr) {
    lda_imm(absolute_x(addr));
}

inline void lda_absy(u16 addr) {
    lda_imm(absolute_y(addr));
}

inline void lda_indx(u8 addr) {
    lda_imm(indirect_x_val(addr));
}

inline void lda_indy(u8 addr) {
    lda_imm(indirect_y_val(addr));
}

// LDX - Load X Register

inline void ldx_imm(u8 value) {
    x = value;
    cpu_update_nz(x);
}

inline void ldx_zp(u8 addr) {
    ldx_imm(zero_page(addr));
}

inline void ldx_zpy(u8 addr) {
    ldx_imm(zero_page_y(addr));
}

inline void ldx_abs(u16 addr) {
    ldx_imm(absolute(addr));
}

inline void ldx_absy(u16 addr) {
    ldx_imm(absolute_y(addr));
}

// LDY - Load Y Register

inline void ldy_imm(u8 value) {
    y = value;
    cpu_update_nz(y);
}

inline void ldy_zp(u8 addr) {
    ldy_imm(zero_page(addr));
}

inline void ldy_zpx(u8 addr) {
    ldy_imm(zero_page_x(addr));
}

inline void ldy_abs(u16 addr) {
    ldy_imm(absolute(addr));
}

inline void ldy_absx(u16 addr) {
    ldy_imm(absolute_x(addr));
}

// STA - Store Accumulator

inline void sta_zp(u8 addr) {
    cpu_write_byte(addr, a);
}

inline void sta_zpx(u8 addr) {
    cpu_write_byte(addr + x, a);
}

inline void sta_zpy(u8 addr) {
    cpu_write_byte(addr + y, a);
}

inline void sta_abs(u16 addr) {
    cpu_write_byte(addr, a);
}

inline void sta_absx(u16 addr) {
    cpu_write_byte(addr + x, a);
}

inline void sta_absy(u16 addr) {
    cpu_write_byte(addr + y, a);
}

inline void sta_indx(u8 addr) {
    cpu_write_byte(indirect_x_addr(addr), a);
}

inline void sta_indy(u8 addr) {
    cpu_write_byte(indirect_y_addr(addr), a);
}

// STX - Store X Register

inline void stx_zp(u8 addr) {
    cpu_write_byte(addr, x);
}

inline void stx_zpy(u8 addr) {
    cpu_write_byte(addr + y, x);
}

inline void stx_abs(u16 addr) {
    cpu_write_byte(addr, x);
}

inline void stx_abs_y(u16 addr) {
    cpu_write_byte(addr + y, x);
}

// STY - Store Y Register

inline void sty_zp(u8 addr) {
    cpu_write_byte(addr, y);
}

inline void sty_zpx(u8 addr) {
    cpu_write_byte(addr + x, y);
}

inline void sty_abs(u16 addr) {
    cpu_write_byte(addr, y);
}

inline void sty_abs_x(u16 addr) {
    cpu_write_byte(addr + x, y);
}

// ADC - Add with Carry

inline void adc_imm(u8 value) {
    u16 sum = a + value + (carry_flag ? 1 : 0);
    carry_flag = sum > 0xff;
    a = (u8)sum;
    cpu_update_nz(a);
}

inline void adc_zp(u8 addr) {
    adc_imm(zero_page(addr));
}

inline void adc_zpx(u8 addr) {
    adc_imm(zero_page_x(addr));
}

inline void adc_zpy(u8 addr) {
    adc_imm(zero_page_y(addr));
}

inline void adc_abs(u16 addr) {
    adc_imm(absolute(addr));
}

inline void adc_absx(u16 addr) {
    adc_imm(absolute_x(addr));
}

inline void adc_absy(u16 addr) {
    adc_imm(absolute_y(addr));
}

inline void adc_indx(u8 addr) {
    adc_imm(indirect_x_val(addr));
}

inline void adc_indy(u8 addr) {
    adc_imm(indirect_y_val(addr));
}

// SBC - Subtract with Carry

inline void sbc_imm(u8 value) {
    u16 diff = a - value - (carry_flag ? 0 : 1);
    carry_flag = diff <= 0xff;
    a = diff & 0xff;
    cpu_update_nz(a);
}

inline void sbc_zp(u8 addr) {
    sbc_imm(zero_page(addr));
}

inline void sbc_zpx(u8 addr) {
    sbc_imm(zero_page_x(addr));
}

inline void sbc_abs(u16 addr) {
    sbc_imm(absolute(addr));
}

inline void sbc_absx(u16 addr) {
    sbc_imm(absolute_x(addr));
}

inline void sbc_absy(u16 addr) {
    sbc_imm(absolute_y(addr));
}

inline void sbc_indx(u8 addr) {
    sbc_imm(indirect_x_val(addr));
}

inline void sbc_indy(u8 addr) {
    sbc_imm(indirect_y_val(addr));
}

// TAX - Transfer Accumulator to X

inline void tax(void) {
    x = a;
    cpu_update_nz(x);
}

// TAY - Transfer Accumulator to Y

inline void tay(void) {
    y = a;
    cpu_update_nz(y);
}

// TSX - Transfer Stack Pointer to X

inline void tsx(void) {
    x = sp;
    cpu_update_nz(x);
}

// TXA - Transfer X to Accumulator

inline void txa(void) {
    a = x;
    cpu_update_nz(a);
}

// TXS - Transfer X to Stack Pointer

inline void txs(void) {
    sp = x;
}

// TYA - Transfer Y to Accumulator

inline void tya(void) {
    a = y;
    cpu_update_nz(a);
}

// AND - Logical AND

inline void and_imm(u8 value) {
    a &= value;
    cpu_update_nz(a);
}

inline void and_zp(u8 addr) {
    and_imm(zero_page(addr));
}

inline void and_zpx(u8 addr) {
    and_imm(zero_page_x(addr));
}

inline void and_abs(u16 addr) {
    and_imm(absolute(addr));
}

inline void and_absx(u16 addr) {
    and_imm(absolute_x(addr));
}

inline void and_absy(u16 addr) {
    and_imm(absolute_y(addr));
}

inline void and_indx(u8 addr) {
    and_imm(indirect_x_val(addr));
}

inline void and_indy(u8 addr) {
    and_imm(indirect_y_val(addr));
}

// ORA - Logical Inclusive OR

inline void ora_imm(u8 value) {
    a |= value;
    cpu_update_nz(a);
}

inline void ora_zp(u8 addr) {
    ora_imm(zero_page(addr));
}

inline void ora_zpx(u8 addr) {
    ora_imm(zero_page_x(addr));
}

inline void ora_zpy(u8 addr) {
    ora_imm(zero_page_y(addr));
}

inline void ora_abs(u16 addr) {
    ora_imm(absolute(addr));
}

inline void ora_absx(u16 addr) {
    ora_imm(absolute_x(addr));
}

inline void ora_absy(u16 addr) {
    ora_imm(absolute_y(addr));
}

inline void ora_indx(u8 addr) {
    ora_imm(indirect_x_val(addr));
}

inline void ora_indy(u8 addr) {
    ora_imm(indirect_y_val(addr));
}

// EOR - Exclusive OR

inline void eor_imm(u8 value) {
    a ^= value;
    cpu_update_nz(a);
}

inline void eor_zp(u8 addr) {
    eor_imm(zero_page(addr));
}

inline void eor_zpx(u8 addr) {
    eor_imm(zero_page_x(addr));
}

inline void eor_abs(u16 addr) {
    eor_imm(absolute(addr));
}

inline void eor_absx(u16 addr) {
    eor_imm(absolute_x(addr));
}

inline void eor_absy(u16 addr) {
    eor_imm(absolute_y(addr));
}

inline void eor_indx(u8 addr) {
    eor_imm(indirect_x_val(addr));
}

inline void eor_indy(u8 addr) {
    eor_imm(indirect_y_val(addr));
}

// ASL - Arithmetic Shift Left

inline void asl_acc(void) {
    carry_flag = a & 0x80;
    a <<= 1;
    cpu_update_nz(a);
}

inline void asl_abs(u16 addr) {
    u8 val = cpu_read_byte(addr);
    carry_flag = val & 0x80;
    val <<= 1;
    cpu_write_byte(addr, val);
    cpu_update_nz(val);
}

inline void asl_zp(u8 addr) {
    asl_abs(addr);
}

inline void asl_zpx(u8 addr) {
    asl_abs(addr + x);
}

inline void asl_absx(u16 addr) {
    asl_abs(addr + x);
}

// LSR - Logical Shift Right

inline void lsr_acc(void) {
    carry_flag = a & 1;
    a >>= 1;
    cpu_update_nz(a);
}

inline void _lsr(u16 addr) {
    u8 val = cpu_read_byte(addr);
    carry_flag = val & 1;
    val >>= 1;
    cpu_write_byte(addr, val);
    cpu_update_nz(val);
}

inline void lsr_zp(u8 addr) {
    _lsr(addr);
}

inline void lsr_abs(u16 addr) {
    _lsr(addr);
}

inline void lsr_zpx(u8 addr) {
    _lsr(addr + x);
}

inline void lsr_absx(u16 addr) {
    _lsr(addr + x);
}

// INC - Increment Memory

inline void _inc(u16 addr) {
    u8 val = cpu_read_byte(addr);
    val++;
    cpu_write_byte(addr, val);
    cpu_update_nz(val);
}

inline void inc_zp(u8 addr) {
    _inc(addr);
}

inline void inc_zpx(u8 addr) {
    _inc(addr + x);
}

inline void inc_abs(u16 addr) {
    _inc(addr);
}

inline void inc_absx(u16 addr) {
    _inc(addr + x);
}

// INX - Increment X Register

inline void inx(void) {
    x++;
    cpu_update_nz(x);
}

// INY - Increment Y Register

inline void iny(void) {
    y++;
    cpu_update_nz(y);
}

// DEC - Decrement Memory

inline void _dec(u16 addr) {
    u8 val = cpu_read_byte(addr);
    val--;
    cpu_write_byte(addr, val);
    cpu_update_nz(val);
}

inline void dec_zp(u8 addr) {
    _dec(addr);
}

inline void dec_zpx(u8 addr) {
    _dec(addr + x);
}

inline void dec_abs(u16 addr) {
    _dec(addr);
}

inline void dec_absx(u16 addr) {
    _dec(addr + x);
}

// DEX - Decrement X Register

inline void dex(void) {
    x--;
    cpu_update_nz(x);
}

// DEY - Decrement Y Register

inline void dey(void) {
    y--;
    cpu_update_nz(y);
}

// CLC - Clear Carry Flag

inline void clc(void) {
    carry_flag = false;
}

// SEC - Set Carry Flag

inline void sec(void) {
    carry_flag = true;
}

// CLD - Clear Decimal Flag

inline void cld(void) {}

// CLV - Clear Overflow Flag
inline void clv(void) {
    overflow_flag = false;
}

// CLI - Clear Interrupt Disable

inline void cli(void) {
    interrupt_disable_flag = false;
}

// SED - Set Decimal Flag

inline void sed(void) {}

// SEI - Set Interrupt Disable

inline void sei(void) {
    interrupt_disable_flag = true;
}

inline void cmp_vals(u8 lhs, u8 rhs) {
    carry_flag = lhs >= rhs;
    cpu_update_nz(lhs - rhs);
}

// CMP - Compare

inline void cmp_imm(u8 value) {
    cmp_vals(a, value);
}

inline void cmp_zp(u8 addr) {
    cmp_vals(a, zero_page(addr));
}

inline void cmp_zpx(u8 addr) {
    cmp_vals(a, zero_page_x(addr));
}

inline void cmp_zpy(u8 addr) {
    cmp_vals(a, zero_page_y(addr));
}

inline void cmp_abs(u16 addr) {
    cmp_vals(a, absolute(addr));
}

inline void cmp_absx(u16 addr) {
    cmp_vals(a, absolute_x(addr));
}

inline void cmp_absy(u16 addr) {
    cmp_vals(a, absolute_y(addr));
}

inline void cmp_indx(u8 addr) {
    cmp_vals(a, indirect_x_val(addr));
}

inline void cmp_indy(u8 addr) {
    cmp_vals(a, indirect_y_val(addr));
}

// CPX - Compare X Register

inline void cpx_imm(u8 value) {
    cmp_vals(x, value);
}

inline void cpx_zp(u8 addr) {
    cmp_vals(x, zero_page(addr));
}

inline void cpx_abs(u16 addr) {
    cmp_vals(x, absolute(addr));
}

// CPY - Compare Y Register

inline void cpy_imm(u8 value) {
    cmp_vals(y, value);
}

inline void cpy_zp(u8 addr) {
    cmp_vals(y, zero_page(addr));
}

inline void cpy_abs(u16 addr) {
    cmp_vals(y, absolute(addr));
}

// PHA - Push Accumulator

inline void pha(void) {
    cpu_write_byte(sp | 0x100, a);
    sp--;
}

// PLA - Pull Accumulator

inline void pla(void) {
    sp++;
    a = cpu_read_byte(sp | 0x100);
    cpu_update_nz(a);
}

// BIT - Bit Test

inline void _bit(u8 val) {
    zero_flag = (a & val) == 0;
    neg_flag = val & 0x80;
}

inline void bit_zp(u8 addr) {
    _bit(zero_page(addr));
}

inline void bit_abs(u16 addr) {
    _bit(absolute(addr));
}

// ROL - Rotate Left

inline void _rol(u16 addr) {
    u8 val = cpu_read_byte(addr);
    bool next_carry_flag = val & 0x80;
    val <<= 1;
    val |= carry_flag;
    carry_flag = next_carry_flag;
    cpu_write_byte(addr, val);
    cpu_update_nz(val);
}

inline void rol_acc(void) {
    bool next_carry_flag = a & 0x80;
    a <<= 1;
    a |= carry_flag;
    carry_flag = next_carry_flag;
    cpu_update_nz(a);
}

inline void rol_zp(u8 addr) {
    _rol(addr);
}

inline void rol_zpx(u8 addr) {
    _rol(addr + x);
}

inline void rol_abs(u16 addr) {
    _rol(addr);
}

inline void rol_absx(u16 addr) {
    _rol(addr + x);
}

// ROR - Rotate Right

void _ror(u16 addr) {
    u8 val = cpu_read_byte(addr);
    bool old_carry = carry_flag;
    carry_flag = val & 1;
    val >>= 1;

    if (old_carry) {
        val |= 0x80;
    }

    cpu_write_byte(addr, val);
    cpu_update_nz(val);
}

inline void ror_acc(void) {
    bool old_carry = carry_flag;
    carry_flag = a & 1;
    a >>= 1;

    if (old_carry) {
        a |= 0x80;
    }

    cpu_update_nz(a);
}

void ror_zp(u8 addr) {
    _ror(zero_page(addr));
}

void ror_zpx(u8 addr) {
    _ror(zero_page_x(addr));
}

void ror_abs(u16 addr) {
    _ror(addr);
}

inline void ror_absx(u16 addr) {
    u8 val = absolute_x(addr);
    bool old_carry = carry_flag;
    carry_flag = val & 1;
    val >>= 1;

    if (old_carry) {
        val |= 0x80;
    }

    cpu_write_byte(addr, val);
    cpu_update_nz(val);
}

// stack instructions

void php(void){
    brk_flag = true;
    cpu_push(cpu_get_flags());
}

void plp(void) {
    u8 flags = cpu_pull();
    flags &= 0b11101111;
    flags |= 0b00100000;
    cpu_set_flags(flags);
}

void jsr(void) {
    u16 ret_addr = pc + 1;
    cpu_push_word(ret_addr);
    u16 target_addr = cpu_read_word(pc);
    pc = target_addr;
}

void rts(void) {
    u16 ret_addr = cpu_pull_word() + 1;
    pc = ret_addr;
}

void rti(void) {
    plp();
    pc = cpu_pull_word();
}

// interrupts

void brk(void) {
    cpu_push_word(pc);
    php();
    sei();
    pc = cpu_read_word(CPU_IRQ_VECTOR);
}

void nmi(void) {
    cpu_push_word(pc);
    php();
    sei();
    pc = cpu_read_word(CPU_NMI_VECTOR);
    cycles += 7;
}

void irq(void) {
    brk();
    cycles += 7;
}

inline void nop(void) {}

// branch instructions

inline void jmp_abs(u16 addr) {
    pc = addr;
}

void jmp_ind(u16 addr) {
    u16 target_addr = cpu_read_word(addr);
    jmp_abs(target_addr);
}

inline bool page_boundary_crossed(u16 prev, u16 next) {
    return ((prev & 0xFF00) != (next & 0xFF00));
}

inline void branch_rel(u8 offset) {
    s8 rel = (s8)offset;
    u16 jump_addr = (u16)((s16)pc + (s16)rel);
    u16 prev_pc = pc;
    pc = jump_addr;
    cycles += 1 + page_boundary_crossed(prev_pc, jump_addr);
}

void bcc_rel(u8 offset) {
    if (!carry_flag) {
        branch_rel(offset);
    }
}

void bcs_rel(u8 offset) {
    if (carry_flag) {
        branch_rel(offset);
    }
}

void beq_rel(u8 offset) {
    if (zero_flag) {
        branch_rel(offset);
    }
}

void bne_rel(u8 offset) {
    if (!zero_flag) {
        branch_rel(offset);
    }
}

void bpl_rel(u8 offset) {
    if (!neg_flag) {
        branch_rel(offset);
    }
}

void bmi_rel(u8 offset) {
    if (neg_flag) {
        branch_rel(offset);
    }
}

void bvc_rel(u8 offset) {
    if (!overflow_flag) {
        branch_rel(offset);
    }
}

void bvs_rel(u8 offset) {
    if (overflow_flag) {
        branch_rel(offset);
    }
}
