#include "cpu.h"
#include "apu.h"
#include <stdio.h> // for printf
#include <stdlib.h> // for exit

const usize INST_CYCLES[] = {
    7, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 4, 4, 6, 6, 2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 4, 4, 6, 6, 2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    6, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 3, 4, 6, 6, 2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 5, 4, 6, 6, 2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4, 2, 6, 2, 6, 4, 4, 4, 4, 2, 5, 2, 5, 5, 5, 5, 5,
    2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4, 2, 5, 2, 5, 4, 4, 4, 4, 2, 4, 2, 4, 4, 4, 4, 4,
    2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6, 2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6, 2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
};

const char* INST_OPCODES[] = {
    "BRK", "ORA", "ILL", "ILL", "ILL", "ORA", "ASL", "ILL", "PHP", "ORA", "ASL", "ILL", "ILL", "ORA", "ASL", "ILL",
    "BPL", "ORA", "ILL", "ILL", "ILL", "ORA", "ASL", "ILL", "CLC", "ORA", "ILL", "ILL", "ILL", "ORA", "ASL", "ILL",
    "JSR", "AND", "ILL", "ILL", "BIT", "AND", "ROL", "ILL", "PLP", "AND", "ROL", "ILL", "BIT", "AND", "ROL", "ILL",
    "BMI", "AND", "ILL", "ILL", "ILL", "AND", "ROL", "ILL", "SEC", "AND", "ILL", "ILL", "ILL", "AND", "ROL", "ILL",
    "RTI", "EOR", "ILL", "ILL", "ILL", "EOR", "LSR", "ILL", "PHA", "EOR", "LSR", "ILL", "JMP", "EOR", "LSR", "ILL",
    "BVC", "EOR", "ILL", "ILL", "ILL", "EOR", "LSR", "ILL", "CLI", "EOR", "ILL", "ILL", "ILL", "EOR", "LSR", "ILL",
    "RTS", "ADC", "ILL", "ILL", "ILL", "ADC", "ROR", "ILL", "PLA", "ADC", "ROR", "ILL", "JMP", "ADC", "ROR", "ILL",
    "BVS", "ADC", "ILL", "ILL", "ILL", "ADC", "ROR", "ILL", "SEI", "ADC", "ILL", "ILL", "ILL", "ADC", "ROR", "ILL",
    "ILL", "STA", "ILL", "ILL", "STY", "STA", "STX", "ILL", "DEY", "ILL", "TXA", "ILL", "STY", "STA", "STX", "ILL",
    "BCC", "STA", "ILL", "ILL", "STY", "STA", "STX", "ILL", "TYA", "STA", "TXS", "ILL", "ILL", "STA", "ILL", "ILL",
    "LDY", "LDA", "LDX", "ILL", "LDY", "LDA", "LDX", "ILL", "TAY", "LDA", "TAX", "ILL", "LDY", "LDA", "LDX", "ILL",
    "BCS", "LDA", "ILL", "ILL", "LDY", "LDA", "LDX", "ILL", "CLV", "LDA", "TSX", "ILL", "LDY", "LDA", "LDX", "ILL",
    "CPY", "CMP", "ILL", "ILL", "CPY", "CMP", "DEC", "ILL", "INY", "CMP", "DEX", "ILL", "CPY", "CMP", "DEC", "ILL",
    "BNE", "CMP", "ILL", "ILL", "ILL", "CMP", "DEC", "ILL", "CLD", "CMP", "ILL", "ILL", "ILL", "CMP", "DEC", "ILL",
    "CPX", "SBC", "ILL", "ILL", "CPX", "SBC", "INC", "ILL", "INX", "SBC", "NOP", "ILL", "CPX", "SBC", "INC", "ILL",
    "BEQ", "SBC", "ILL", "ILL", "ILL", "SBC", "INC", "ILL", "SED", "SBC", "ILL", "ILL", "ILL", "SBC", "INC", "ILL"
};

void cpu_set_flags(CPU* self, u8 flags);

void cpu_init(CPU* self, Cart cart, PPU* ppu, APU* apu, Mapper* mapper) {
    self->ppu = ppu;
    self->apu = apu;
    self->cart = cart;
    self->mapper = mapper;

    // registers
    self->a = 0;
    self->x = 0;
    self->y = 0;
    self->sp = CPU_STACK_TOP;
    self->pc = cpu_read_word(self, CPU_RESET_VECTOR);

    // flags
    cpu_set_flags(self, 0x24);

    // controller
    self->controller1_state = 0;
    self->controller1_strobe = false;
    self->controller1_btn_index = 0;

    self->inst_cycles = 0;
    self->total_cycles = 0;
    self->stall_cycles = 0;

    memset(self->ram, 0, sizeof(self->ram));
}

void cpu_free(CPU* self) {}

u8 cpu_read_byte(CPU* self, u16 addr) {
    if (addr < 0x2000) {
        return self->ram[addr & 0x7ff];
    }

    if (addr < 0x4000) {
        return ppu_read_register(self->ppu, 0x2000 + (addr & 7));
    }

    if (addr == 0x4016) {
        if (self->controller1_btn_index > 7) {
            return 1;
        }

        u8 state = (self->controller1_state & (1 << self->controller1_btn_index)) >> self->controller1_btn_index;

        if (!self->controller1_strobe && self->controller1_btn_index < 8) {
            self->controller1_btn_index++;
        }
        
        return state;
    }

    if (addr < 0x4020) {
        // APU
        return 0;
    }

    return self->mapper->read(self->mapper, addr);
}

void cpu_transfer_oam(CPU* self, u16 start_addr) {
    memcpy(self->ppu->oam, self->ram + start_addr, 256);
    self->stall_cycles += 513 + (self->total_cycles & 1);
}

void cpu_write_byte(CPU* self, u16 addr, u8 value) {
    if (addr < 0x2000) {
        self->ram[addr & 0x7ff] = value;
    } else if (addr < 0x4000) {
        ppu_write_register(self->ppu, 0x2000 + (addr & 7), value);
    } else if (addr == 0x4014) {
        u16 start_addr = (u16)(value << 8);
        cpu_transfer_oam(self, start_addr);
    } else if (addr == 0x4016) {
        // controller 1
        self->controller1_strobe = (value & 1) == 1;
        
        if (self->controller1_strobe) {
            self->controller1_btn_index = 0;
        }
    } else if (addr < 0x4020) {
        apu_write(self->apu, addr, value);
    } else {
        self->mapper->write(self->mapper, addr, value);
    }
}

u16 cpu_read_word(CPU* self, u16 addr) {
    // little endian
    u16 low_byte = (u16)cpu_read_byte(self, addr);
    u16 high_byte = (u16)(((u16)cpu_read_byte(self, addr + 1)) << 8);
    u16 word = high_byte | low_byte;
    return word;
}

void cpu_update_controller1(CPU* self, u8 state) {
    self->controller1_state = state;
}

void cpu_write_word(CPU* self, u16 addr, u16 value) {
    cpu_write_byte(self, addr, value & 0xff);
    cpu_write_byte(self, addr + 1, value >> 8);
}

// addressing mode utils

void cpu_update_nz(CPU* self, u8 value) {
    self->zero_flag = value == 0;
    self->neg_flag = (value & 0b010000000) != 0;
}

u8 zero_page(CPU* self, u8 addr) {
    return cpu_read_byte(self, (u16)addr);
}

u8 zero_page_x_addr(CPU* self, u8 addr) {
    return addr + self->x;
}

u8 zero_page_x(CPU* self, u8 addr) {
    return cpu_read_byte(self, (u16)zero_page_x_addr(self, addr));
}

u8 zero_page_y_addr(CPU* self, u8 addr) {
    return addr + self->y;
}

u8 zero_page_y(CPU* self, u8 addr) {
    return cpu_read_byte(self, (u16)zero_page_y_addr(self, addr));
}

u8 absolute(CPU* self, u16 addr) {
    return cpu_read_byte(self, addr);
}

u16 absolute_x_addr(CPU* self, u16 addr) {
    return addr + self->x;
}

u8 absolute_x(CPU* self, u16 addr) {
    return cpu_read_byte(self, absolute_x_addr(self, addr));
}

u8 absolute_y(CPU* self, u16 addr) {
    return cpu_read_byte(self, addr + self->y);
}

u16 indirect_x_addr(CPU* self, u8 addr) {
    u8 addr1 = addr + self->x;
    u8 addr2 = addr1 + 1; // zero page wrap around
    u16 low_byte = (u16)cpu_read_byte(self, addr1);
    u16 high_byte = (u16)(((u16)cpu_read_byte(self, addr2)) << 8);
    return high_byte | low_byte;
    return cpu_read_word(self, addr1);
}

u16 indirect_y_addr(CPU* self, u8 addr) {
    u8 addr2 = addr + 1; // zero page wrap around
    u16 low_byte = (u16)cpu_read_byte(self, addr);
    u16 high_byte = (u16)(((u16)cpu_read_byte(self, addr2)) << 8);
    return (high_byte | low_byte) + (u16)self->y;
}

u8 indirect_x_val(CPU* self, u8 addr) {
    return cpu_read_byte(self, indirect_x_addr(self, addr));
}

u8 indirect_y_val(CPU* self, u8 addr) {
    return cpu_read_byte(self, indirect_y_addr(self, addr));
}

void cpu_push(CPU* self, u8 value) {
    cpu_write_byte(self, (u16)(CPU_STACK_START + self->sp), value);
    self->sp--;
}

void cpu_push_word(CPU* self, u16 value) {
    cpu_push(self, (u8)(value >> 8));
    cpu_push(self, (u8)(value & 0xff));
}

u8 cpu_pull(CPU* self) {
    self->sp++;
    return cpu_read_byte(self, (u16)(CPU_STACK_START + self->sp));
}

u16 cpu_pull_word(CPU* self) {
    u8 low_byte = cpu_pull(self);
    u8 high_byte = cpu_pull(self);
    return (u16)((high_byte << 8) | low_byte);
}

u8 cpu_next_byte(CPU* self) {
    u8 byte = cpu_read_byte(self, self->pc);
    self->pc++;
    return byte;
}

u16 cpu_next_word(CPU* self) {
    u16 low_byte = (u16)cpu_next_byte(self);
    u16 high_byte = (u16)cpu_next_byte(self);
    return (u16)((high_byte << 8) | low_byte);
}

u8 cpu_get_flags(CPU* self) {
    // NV1BDIZC
    u8 flags = 0;
    flags |= self->carry_flag;
    flags |= self->zero_flag << 1;
    flags |= self->interrupt_disable_flag << 2;
    flags |= self->decimal_flag << 3;
    flags |= self->break_flag << 4;
    flags |= 1 << 5;
    flags |= self->overflow_flag << 6;
    flags |= self->neg_flag << 7;

    return flags;
}

void cpu_set_flags(CPU* self, u8 flags) {
    self->carry_flag = flags & 1;
    self->zero_flag = (flags >> 1) & 1;
    self->interrupt_disable_flag = (flags >> 2) & 1;
    self->decimal_flag = (flags >> 3) & 1;
    self->break_flag = (flags >> 4) & 1;
    self->overflow_flag = (flags >> 6) & 1;
    self->neg_flag = (flags >> 7) & 1;
}

// LDA - Load Accumulator

void lda_imm(CPU* self, u8 value) {
    self->a = value;
    cpu_update_nz(self, self->a);
}

void lda_zp(CPU* self, u8 addr) {
    lda_imm(self, zero_page(self, addr));
}

void lda_zpx(CPU* self, u8 addr) {
    lda_imm(self, zero_page_x(self, addr));
}

void lda_zpy(CPU* self, u8 addr) {
    lda_imm(self, zero_page_y(self, addr));
}

void lda_abs(CPU* self, u16 addr) {
    lda_imm(self, absolute(self, addr));
}

void lda_absx(CPU* self, u16 addr) {
    lda_imm(self, absolute_x(self, addr));
}

void lda_absy(CPU* self, u16 addr) {
    lda_imm(self, absolute_y(self, addr));
}

void lda_indx(CPU* self, u8 addr) {
    lda_imm(self, indirect_x_val(self, addr));
}

void lda_indy(CPU* self, u8 addr) {
    lda_imm(self, indirect_y_val(self, addr));
}

// LDX - Load X Register

void ldx_imm(CPU* self, u8 value) {
    self->x = value;
    cpu_update_nz(self, self->x);
}

void ldx_zp(CPU* self, u8 addr) {
    ldx_imm(self, zero_page(self, addr));
}

void ldx_zpy(CPU* self, u8 addr) {
    ldx_imm(self, zero_page_y(self, addr));
}

void ldx_abs(CPU* self, u16 addr) {
    ldx_imm(self, absolute(self, addr));
}

void ldx_absy(CPU* self, u16 addr) {
    ldx_imm(self, absolute_y(self, addr));
}

// LDY - Load Y Register

void ldy_imm(CPU* self, u8 value) {
    self->y = value;
    cpu_update_nz(self, self->y);
}

void ldy_zp(CPU* self, u8 addr) {
    ldy_imm(self, zero_page(self, addr));
}

void ldy_zpx(CPU* self, u8 addr) {
    ldy_imm(self, zero_page_x(self, addr));
}

void ldy_abs(CPU* self, u16 addr) {
    ldy_imm(self, absolute(self, addr));
}

void ldy_absx(CPU* self, u16 addr) {
    ldy_imm(self, absolute_x(self, addr));
}

// STA - Store Accumulator

void sta_zp(CPU* self, u8 addr) {
    cpu_write_byte(self, (u16)addr, self->a);
}

void sta_zpx(CPU* self, u8 addr) {
    cpu_write_byte(self, (u16)zero_page_x_addr(self, addr), self->a);
}

void sta_zpy(CPU* self, u8 addr) {
    cpu_write_byte(self, (u16)zero_page_y_addr(self, addr), self->a);
}

void sta_abs(CPU* self, u16 addr) {
    cpu_write_byte(self, addr, self->a);
}

void sta_absx(CPU* self, u16 addr) {
    cpu_write_byte(self, addr + self->x, self->a);
}

void sta_absy(CPU* self, u16 addr) {
    cpu_write_byte(self, addr + self->y, self->a);
}

void sta_indx(CPU* self, u8 addr) {
    cpu_write_byte(self, indirect_x_addr(self, addr), self->a);
}

void sta_indy(CPU* self, u8 addr) {
    cpu_write_byte(self, indirect_y_addr(self, addr), self->a);
}

// STX - Store X Register

void stx_zp(CPU* self, u8 addr) {
    cpu_write_byte(self, addr, self->x);
}

void stx_zpy(CPU* self, u8 addr) {
    cpu_write_byte(self, zero_page_y_addr(self, addr), self->x);
}

void stx_abs(CPU* self, u16 addr) {
    cpu_write_byte(self, addr, self->x);
}

void stx_abs_y(CPU* self, u16 addr) {
    cpu_write_byte(self, addr + self->y, self->x);
}

// STY - Store Y Register

void sty_zp(CPU* self, u8 addr) {
    cpu_write_byte(self, addr, self->y);
}

void sty_zpx(CPU* self, u8 addr) {
    cpu_write_byte(self, (u16)zero_page_x_addr(self, addr), self->y);
}

void sty_abs(CPU* self, u16 addr) {
    cpu_write_byte(self, addr, self->y);
}

void sty_abs_x(CPU* self, u16 addr) {
    cpu_write_byte(self, addr + self->x, self->y);
}

// ADC - Add with Carry

void adc_imm(CPU* self, u8 value) {
    u16 sum = (u16)self->a + (u16)value + (u16)self->carry_flag;
    self->carry_flag = sum > 0xff;
    u8 sum_u8 = (u8)sum;
    // http://www.6502.org/tutorials/vflag.html
    self->overflow_flag = ((value ^ sum_u8) & (sum_u8 ^ self->a) & 0x80) != 0;
    self->a = sum_u8;
    cpu_update_nz(self, self->a);
}

void adc_zp(CPU* self, u8 addr) {
    adc_imm(self, zero_page(self, addr));
}

void adc_zpx(CPU* self, u8 addr) {
    adc_imm(self, zero_page_x(self, addr));
}

void adc_zpy(CPU* self, u8 addr) {
    adc_imm(self, zero_page_y(self, addr));
}

void adc_abs(CPU* self, u16 addr) {
    adc_imm(self, absolute(self, addr));
}

void adc_absx(CPU* self, u16 addr) {
    adc_imm(self, absolute_x(self, addr));
}

void adc_absy(CPU* self, u16 addr) {
    adc_imm(self, absolute_y(self, addr));
}

void adc_indx(CPU* self, u8 addr) {
    adc_imm(self, indirect_x_val(self, addr));
}

void adc_indy(CPU* self, u8 addr) {
    adc_imm(self, indirect_y_val(self, addr));
}

// SBC - Subtract with Carry

void sbc_imm(CPU* self, u8 value) {
    u16 diff = self->a - value - (self->carry_flag ? 0 : 1);
    self->carry_flag = diff <= 0xff;
    u8 sum = diff & 0xff;
    self->overflow_flag = ((self->a ^ value) & (self->a ^ sum) & 0x80) == 0x80;
    self->a = sum;
    cpu_update_nz(self, self->a);
}

void sbc_zp(CPU* self, u8 addr) {
    sbc_imm(self, zero_page(self, addr));
}

void sbc_zpx(CPU* self, u8 addr) {
    sbc_imm(self, zero_page_x(self, addr));
}

void sbc_abs(CPU* self, u16 addr) {
    sbc_imm(self, absolute(self, addr));
}

void sbc_absx(CPU* self, u16 addr) {
    sbc_imm(self, absolute_x(self, addr));
}

void sbc_absy(CPU* self, u16 addr) {
    sbc_imm(self, absolute_y(self, addr));
}

void sbc_indx(CPU* self, u8 addr) {
    sbc_imm(self, indirect_x_val(self, addr));
}

void sbc_indy(CPU* self, u8 addr) {
    sbc_imm(self, indirect_y_val(self, addr));
}

// TAX - Transfer Accumulator to X

void tax(CPU* self) {
    self->x = self->a;
    cpu_update_nz(self, self->x);
}

// TAY - Transfer Accumulator to Y

void tay(CPU* self) {
    self->y = self->a;
    cpu_update_nz(self, self->y);
}

// TSX - Transfer Stack Pointer to X

void tsx(CPU* self) {
    self->x = self->sp;
    cpu_update_nz(self, self->x);
}

// TXA - Transfer X to Accumulator

void txa(CPU* self) {
    self->a = self->x;
    cpu_update_nz(self, self->a);
}

// TXS - Transfer X to Stack Pointer

void txs(CPU* self) {
    self->sp = self->x;
}

// TYA - Transfer Y to Accumulator

void tya(CPU* self) {
    self->a = self->y;
    cpu_update_nz(self, self->a);
}

// AND - Logical AND

void and_imm(CPU* self, u8 value) {
    self->a &= value;
    cpu_update_nz(self, self->a);
}

void and_zp(CPU* self, u8 addr) {
    and_imm(self, zero_page(self, addr));
}

void and_zpx(CPU* self, u8 addr) {
    and_imm(self, zero_page_x(self, addr));
}

void and_abs(CPU* self, u16 addr) {
    and_imm(self, absolute(self, addr));
}

void and_absx(CPU* self, u16 addr) {
    and_imm(self, absolute_x(self, addr));
}

void and_absy(CPU* self, u16 addr) {
    and_imm(self, absolute_y(self, addr));
}

void and_indx(CPU* self, u8 addr) {
    and_imm(self, indirect_x_val(self, addr));
}

void and_indy(CPU* self, u8 addr) {
    and_imm(self, indirect_y_val(self, addr));
}

// ORA - Logical Inclusive OR

void ora_imm(CPU* self, u8 value) {
    self->a |= value;
    cpu_update_nz(self, self->a);
}

void ora_zp(CPU* self, u8 addr) {
    ora_imm(self, zero_page(self, addr));
}

void ora_zpx(CPU* self, u8 addr) {
    ora_imm(self, zero_page_x(self, addr));
}

void ora_zpy(CPU* self, u8 addr) {
    ora_imm(self, zero_page_y(self, addr));
}

void ora_abs(CPU* self, u16 addr) {
    ora_imm(self, absolute(self, addr));
}

void ora_absx(CPU* self, u16 addr) {
    ora_imm(self, absolute_x(self, addr));
}

void ora_absy(CPU* self, u16 addr) {
    ora_imm(self, absolute_y(self, addr));
}

void ora_indx(CPU* self, u8 addr) {
    ora_imm(self, indirect_x_val(self, addr));
}

void ora_indy(CPU* self, u8 addr) {
    ora_imm(self, indirect_y_val(self, addr));
}

// EOR - Exclusive OR

void eor_imm(CPU* self, u8 value) {
    self->a ^= value;
    cpu_update_nz(self, self->a);
}

void eor_zp(CPU* self, u8 addr) {
    eor_imm(self, zero_page(self, addr));
}

void eor_zpx(CPU* self, u8 addr) {
    eor_imm(self, zero_page_x(self, addr));
}

void eor_abs(CPU* self, u16 addr) {
    eor_imm(self, absolute(self, addr));
}

void eor_absx(CPU* self, u16 addr) {
    eor_imm(self, absolute_x(self, addr));
}

void eor_absy(CPU* self, u16 addr) {
    eor_imm(self, absolute_y(self, addr));
}

void eor_indx(CPU* self, u8 addr) {
    eor_imm(self, indirect_x_val(self, addr));
}

void eor_indy(CPU* self, u8 addr) {
    eor_imm(self, indirect_y_val(self, addr));
}

// ASL - Arithmetic Shift Left

void asl_acc(CPU* self) {
    self->carry_flag = self->a & 0x80;
    self->a <<= 1;
    cpu_update_nz(self, self->a);
}

void asl_abs(CPU* self, u16 addr) {
    u8 val = cpu_read_byte(self, addr);
    self->carry_flag = val & 0x80;
    val <<= 1;
    cpu_write_byte(self, addr, val);
    cpu_update_nz(self, val);
}

void asl_zp(CPU* self, u8 addr) {
    asl_abs(self, addr);
}

void asl_zpx(CPU* self, u8 addr) {
    asl_abs(self, addr + self->x);
}

void asl_absx(CPU* self, u16 addr) {
    asl_abs(self, addr + self->x);
}

// LSR - Logical Shift Right

void lsr_acc(CPU* self) {
    self->carry_flag = self->a & 1;
    self->a >>= 1;
    cpu_update_nz(self, self->a);
}

void _lsr(CPU* self, u16 addr) {
    u8 val = cpu_read_byte(self, addr);
    self->carry_flag = val & 1;
    val >>= 1;
    cpu_write_byte(self, addr, val);
    cpu_update_nz(self, val);
}

void lsr_zp(CPU* self, u8 addr) {
    _lsr(self, addr);
}

void lsr_abs(CPU* self, u16 addr) {
    _lsr(self, addr);
}

void lsr_zpx(CPU* self, u8 addr) {
    _lsr(self, addr + self->x);
}

void lsr_absx(CPU* self, u16 addr) {
    _lsr(self, addr + self->x);
}

// INC - Increment Memory

void _inc(CPU* self, u16 addr) {
    u8 val = cpu_read_byte(self, addr);
    val++;
    cpu_write_byte(self, addr, val);
    cpu_update_nz(self, val);
}

void inc_zp(CPU* self, u8 addr) {
    _inc(self, addr);
}

void inc_zpx(CPU* self, u8 addr) {
    _inc(self, addr + self->x);
}

void inc_abs(CPU* self, u16 addr) {
    _inc(self, addr);
}

void inc_absx(CPU* self, u16 addr) {
    _inc(self, addr + self->x);
}

// INX - Increment X Register

void inx(CPU* self) {
    self->x++;
    cpu_update_nz(self, self->x);
}

// INY - Increment Y Register

void iny(CPU* self) {
    self->y++;
    cpu_update_nz(self, self->y);
}

// DEC - Decrement Memory

void _dec(CPU* self, u16 addr) {
    u8 val = cpu_read_byte(self, addr);
    val--;
    cpu_write_byte(self, addr, val);
    cpu_update_nz(self, val);
}

void dec_zp(CPU* self, u8 addr) {
    _dec(self, addr);
}

void dec_zpx(CPU* self, u8 addr) {
    _dec(self, addr + self->x);
}

void dec_abs(CPU* self, u16 addr) {
    _dec(self, addr);
}

void dec_absx(CPU* self, u16 addr) {
    _dec(self, addr + self->x);
}

// DEX - Decrement X Register

void dex(CPU* self) {
    self->x--;
    cpu_update_nz(self, self->x);
}

// DEY - Decrement Y Register

void dey(CPU* self) {
    self->y--;
    cpu_update_nz(self, self->y);
}

// CLC - Clear Carry Flag

void clc(CPU* self) {
    self->carry_flag = false;
}

// SEC - Set Carry Flag

void sec(CPU* self) {
    self->carry_flag = true;
}

// CLD - Clear Decimal Flag

void cld(CPU* self) {
    self->decimal_flag = false;
}

// CLV - Clear Overflow Flag
void clv(CPU* self) {
    self->overflow_flag = false;
}

// CLI - Clear Interrupt Disable

void cli(CPU* self) {
    self->interrupt_disable_flag = false;
}

// SED - Set Decimal Flag

void sed(CPU* self) {
    self->decimal_flag = true;
}

// SEI - Set Interrupt Disable

void sei(CPU* self) {
    self->interrupt_disable_flag = true;
}

void cmp_vals(CPU* self, u8 lhs, u8 rhs) {
    self->carry_flag = lhs >= rhs;
    cpu_update_nz(self, lhs - rhs);
}

// CMP - Compare

void cmp_imm(CPU* self, u8 value) {
    cmp_vals(self, self->a, value);
}

void cmp_zp(CPU* self, u8 addr) {
    cmp_vals(self, self->a, zero_page(self, addr));
}

void cmp_zpx(CPU* self, u8 addr) {
    cmp_vals(self, self->a, zero_page_x(self, addr));
}

void cmp_zpy(CPU* self, u8 addr) {
    cmp_vals(self, self->a, zero_page_y(self, addr));
}

void cmp_abs(CPU* self, u16 addr) {
    cmp_vals(self, self->a, absolute(self, addr));
}

void cmp_absx(CPU* self, u16 addr) {
    cmp_vals(self, self->a, absolute_x(self, addr));
}

void cmp_absy(CPU* self, u16 addr) {
    cmp_vals(self, self->a, absolute_y(self, addr));
}

void cmp_indx(CPU* self, u8 addr) {
    cmp_vals(self, self->a, indirect_x_val(self, addr));
}

void cmp_indy(CPU* self, u8 addr) {
    cmp_vals(self, self->a, indirect_y_val(self, addr));
}

// CPX - Compare X Register

void cpx_imm(CPU* self, u8 value) {
    cmp_vals(self, self->x, value);
}

void cpx_zp(CPU* self, u8 addr) {
    cmp_vals(self, self->x, zero_page(self, addr));
}

void cpx_abs(CPU* self, u16 addr) {
    cmp_vals(self, self->x, absolute(self, addr));
}

// CPY - Compare Y Register

void cpy_imm(CPU* self, u8 value) {
    cmp_vals(self, self->y, value);
}

void cpy_zp(CPU* self, u8 addr) {
    cmp_vals(self, self->y, zero_page(self, addr));
}

void cpy_abs(CPU* self, u16 addr) {
    cmp_vals(self, self->y, absolute(self, addr));
}

// PHA - Push Accumulator

void pha(CPU* self) {
    cpu_write_byte(self, self->sp | 0x100, self->a);
    self->sp--;
}

// PLA - Pull Accumulator

void pla(CPU* self) {
    self->sp++;
    self->a = cpu_read_byte(self, self->sp | 0x100);
    cpu_update_nz(self, self->a);
}

// BIT - Bit Test

void _bit(CPU* self, u8 val) {
    self->zero_flag = (self->a & val) == 0;
    self->overflow_flag = val & 0x40;
    self->neg_flag = val & 0x80;
}

void bit_zp(CPU* self, u8 addr) {
    _bit(self, zero_page(self, addr));
}

void bit_abs(CPU* self, u16 addr) {
    _bit(self, absolute(self, addr));
}

// ROL - Rotate Left

void _rol(CPU* self, u16 addr) {
    u8 val = cpu_read_byte(self, addr);
    bool next_carry_flag = val & 0x80;
    val <<= 1;
    val |= self->carry_flag;
    self->carry_flag = next_carry_flag;
    cpu_write_byte(self, addr, val);
    cpu_update_nz(self, val);
}

void rol_acc(CPU* self) {
    bool next_carry_flag = self->a & 0x80;
    self->a <<= 1;
    self->a |= self->carry_flag;
    self->carry_flag = next_carry_flag;
    cpu_update_nz(self, self->a);
}

void rol_zp(CPU* self, u8 addr) {
    _rol(self, addr);
}

void rol_zpx(CPU* self, u8 addr) {
    _rol(self, addr + self->x);
}

void rol_abs(CPU* self, u16 addr) {
    _rol(self, addr);
}

void rol_absx(CPU* self, u16 addr) {
    _rol(self, addr + self->x);
}

// ROR - Rotate Right

void _ror(CPU* self, u16 addr) {
    u8 val = cpu_read_byte(self, addr);
    bool old_carry = self->carry_flag;
    self->carry_flag = val & 1;
    val >>= 1;

    if (old_carry) {
        val |= 0x80;
    }

    cpu_write_byte(self, addr, val);
    cpu_update_nz(self, val);
}

void ror_acc(CPU* self) {
    bool old_carry = self->carry_flag;
    self->carry_flag = self->a & 1;
    self->a >>= 1;

    if (old_carry) {
        self->a |= 0x80;
    }

    cpu_update_nz(self, self->a);
}

void ror_zp(CPU* self, u8 addr) {
    _ror(self, (u16)addr);
}

void ror_zpx(CPU* self, u8 addr) {
    _ror(self, (u16)zero_page_x_addr(self, addr));
}

void ror_abs(CPU* self, u16 addr) {
    _ror(self, addr);
}

void ror_absx(CPU* self, u16 addr) {
    _ror(self, absolute_x_addr(self, addr));
}

// stack instructions

void php(CPU* self){
    cpu_push(self, cpu_get_flags(self) | 0x10);
}

void plp(CPU* self) {
    u8 flags = cpu_pull(self);
    flags &= 0b11101111;
    flags |= 0b00100000;
    cpu_set_flags(self, flags);
}

void jsr(CPU* self) {
    u16 ret_addr = self->pc + 1;
    cpu_push_word(self, ret_addr);
    u16 target_addr = cpu_read_word(self, self->pc);
    self->pc = target_addr;
}

void rts(CPU* self) {
    u16 ret_addr = cpu_pull_word(self) + 1;
    self->pc = ret_addr;
}

void rti(CPU* self) {
    plp(self);
    self->pc = cpu_pull_word(self);
}

// interrupts

void brk(CPU* self) {
    cpu_push_word(self, self->pc);
    php(self);
    sei(self);
    self->pc = cpu_read_word(self, CPU_IRQ_VECTOR);
}

void nmi(CPU* self) {
    cpu_push_word(self, self->pc);
    php(self);
    sei(self);
    self->pc = cpu_read_word(self, CPU_NMI_VECTOR);
    self->inst_cycles += 7;
}

void irq(CPU* self) {
    brk(self);
    self->inst_cycles += 7;
}

void nop(CPU* self) {}

// branch instructions

void jmp_abs(CPU* self, u16 addr) {
    self->pc = addr;
}

void jmp_ind(CPU* self, u16 addr) {
    // An original 6502 does not correctly fetch the target address
    // if the indirect vector falls on a page boundary
    // (e.g. $xxFF where xx is any value from $00 to $FF).
    // In this case fetches the LSB from $xxFF as expected but takes the MSB from $xx00.
    // This is fixed in some later chips like the 65SC02 so for compatibility
    // always ensure the indirect vector is not at the end of the page.

    u16 target_addr;

    if ((addr & 0x00ff) == 0xff) {
        u8 lo = cpu_read_byte(self, addr);
        u8 hi = cpu_read_byte(self, addr & 0xff00);
        target_addr = (u16)(((u16)hi << 8) | (u16)lo);
    } else {
        target_addr = cpu_read_word(self, addr);
    }

    jmp_abs(self, target_addr);
}

bool page_boundary_crossed(u16 prev, u16 next) {
    return ((prev & 0xFF00) != (next & 0xFF00));
}

void branch_rel(CPU* self, u8 offset) {
    s8 rel = (s8)offset;
    u16 jump_addr = (u16)((s16)self->pc + (s16)rel);
    u16 prev_pc = self->pc;
    self->pc = jump_addr;
    self->inst_cycles += 1 + page_boundary_crossed(prev_pc, jump_addr);
}

void bcc_rel(CPU* self, u8 offset) {
    if (!self->carry_flag) {
        branch_rel(self, offset);
    }
}

void bcs_rel(CPU* self, u8 offset) {
    if (self->carry_flag) {
        branch_rel(self, offset);
    }
}

void beq_rel(CPU* self, u8 offset) {
    if (self->zero_flag) {
        branch_rel(self, offset);
    }
}

void bne_rel(CPU* self, u8 offset) {
    if (!self->zero_flag) {
        branch_rel(self, offset);
    }
}

void bpl_rel(CPU* self, u8 offset) {
    if (!self->neg_flag) {
        branch_rel(self, offset);
    }
}

void bmi_rel(CPU* self, u8 offset) {
    if (self->neg_flag) {
        branch_rel(self, offset);
    }
}

void bvc_rel(CPU* self, u8 offset) {
    if (!self->overflow_flag) {
        branch_rel(self, offset);
    }
}

void bvs_rel(CPU* self, u8 offset) {
    if (self->overflow_flag) {
        branch_rel(self, offset);
    }
}

usize cpu_step(CPU* self) {
    if (self->apu->dmc.cpu_stall_cycles > 0) {
        self->apu->dmc.cpu_stall_cycles--;
        return 1;
    } else if (self->stall_cycles > 0) {
        self->stall_cycles--;
        return 1;
    }

    u8 opcode = cpu_read_byte(self, self->pc);
    // printf("PC: %04X, %s, A: %02X, X: %02X, Y: %02X, SP: %02X, c: %zu, f: %zu\n", pc, INST_OPCODES[opcode], self->a, self->x, self->y, sp, cpu_total_cycles, frame_count);

    if (self->ppu->nmi_triggered) {
        self->ppu->nmi_triggered = false;
        nmi(self);
        return 0;
    }

    self->pc++;

    switch (opcode) {
        case 0x00: brk(self); break;
        case 0xEA: nop(self); break;
        case 0xA9: lda_imm(self, cpu_next_byte(self)); break;
        case 0xA5: lda_zp(self, cpu_next_byte(self)); break;
        case 0xB5: lda_zpx(self, cpu_next_byte(self)); break;
        case 0xAD: lda_abs(self, cpu_next_word(self)); break;
        case 0xBD: lda_absx(self, cpu_next_word(self)); break;
        case 0xB9: lda_absy(self, cpu_next_word(self)); break;
        case 0xA1: lda_indx(self, cpu_next_byte(self)); break;
        case 0xB1: lda_indy(self, cpu_next_byte(self)); break;
        case 0xA2: ldx_imm(self, cpu_next_byte(self)); break;
        case 0xA6: ldx_zp(self, cpu_next_byte(self)); break;
        case 0xB6: ldx_zpy(self, cpu_next_byte(self)); break;
        case 0xAE: ldx_abs(self, cpu_next_word(self)); break;
        case 0xBE: ldx_absy(self, cpu_next_word(self)); break;
        case 0xA0: ldy_imm(self, cpu_next_byte(self)); break;
        case 0xA4: ldy_zp(self, cpu_next_byte(self)); break;
        case 0xB4: ldy_zpx(self, cpu_next_byte(self)); break;
        case 0xAC: ldy_abs(self, cpu_next_word(self)); break;
        case 0xBC: ldy_absx(self, cpu_next_word(self)); break;
        case 0x85: sta_zp(self, cpu_next_byte(self)); break;
        case 0x95: sta_zpx(self, cpu_next_byte(self)); break;
        case 0x8D: sta_abs(self, cpu_next_word(self)); break;
        case 0x9D: sta_absx(self, cpu_next_word(self)); break;
        case 0x99: sta_absy(self, cpu_next_word(self)); break;
        case 0x81: sta_indx(self, cpu_next_byte(self)); break;
        case 0x91: sta_indy(self, cpu_next_byte(self)); break;
        case 0x86: stx_zp(self, cpu_next_byte(self)); break;
        case 0x96: stx_zpy(self, cpu_next_byte(self)); break;
        case 0x8E: stx_abs(self, cpu_next_word(self)); break;
        case 0x84: sty_zp(self, cpu_next_byte(self)); break;
        case 0x94: sty_zpx(self, cpu_next_byte(self)); break;
        case 0x8C: sty_abs(self, cpu_next_word(self)); break;
        case 0x69: adc_imm(self, cpu_next_byte(self)); break;
        case 0x65: adc_zp(self, cpu_next_byte(self)); break;
        case 0x75: adc_zpx(self, cpu_next_byte(self)); break;
        case 0x6D: adc_abs(self, cpu_next_word(self)); break;
        case 0x7D: adc_absx(self, cpu_next_word(self)); break;
        case 0x79: adc_absy(self, cpu_next_word(self)); break;
        case 0x61: adc_indx(self, cpu_next_byte(self)); break;
        case 0x71: adc_indy(self, cpu_next_byte(self)); break;
        case 0xE9: sbc_imm(self, cpu_next_byte(self)); break;
        case 0xE5: sbc_zp(self, cpu_next_byte(self)); break;
        case 0xF5: sbc_zpx(self, cpu_next_byte(self)); break;
        case 0xED: sbc_abs(self, cpu_next_word(self)); break;
        case 0xFD: sbc_absx(self, cpu_next_word(self)); break;
        case 0xF9: sbc_absy(self, cpu_next_word(self)); break;
        case 0xE1: sbc_indx(self, cpu_next_byte(self)); break;
        case 0xF1: sbc_indy(self, cpu_next_byte(self)); break;
        case 0xAA: tax(self); break;
        case 0xA8: tay(self); break;
        case 0xBA: tsx(self); break;
        case 0x8A: txa(self); break;
        case 0x9A: txs(self); break;
        case 0x98: tya(self); break;
        case 0x29: and_imm(self, cpu_next_byte(self)); break;
        case 0x25: and_zp(self, cpu_next_byte(self)); break;
        case 0x35: and_zpx(self, cpu_next_byte(self)); break;
        case 0x2D: and_abs(self, cpu_next_word(self)); break;
        case 0x3D: and_absx(self, cpu_next_word(self)); break;
        case 0x39: and_absy(self, cpu_next_word(self)); break;
        case 0x21: and_indx(self, cpu_next_byte(self)); break;
        case 0x31: and_indy(self, cpu_next_byte(self)); break;
        case 0x09: ora_imm(self, cpu_next_byte(self)); break;
        case 0x05: ora_zp(self, cpu_next_byte(self)); break;
        case 0x15: ora_zpx(self, cpu_next_byte(self)); break;
        case 0x0D: ora_abs(self, cpu_next_word(self)); break;
        case 0x1D: ora_absx(self, cpu_next_word(self)); break;
        case 0x19: ora_absy(self, cpu_next_word(self)); break;
        case 0x01: ora_indx(self, cpu_next_byte(self)); break;
        case 0x11: ora_indy(self, cpu_next_byte(self)); break;
        case 0x49: eor_imm(self, cpu_next_byte(self)); break;
        case 0x45: eor_zp(self, cpu_next_byte(self)); break;
        case 0x55: eor_zpx(self, cpu_next_byte(self)); break;
        case 0x4D: eor_abs(self, cpu_next_word(self)); break;
        case 0x5D: eor_absx(self, cpu_next_word(self)); break;
        case 0x59: eor_absy(self, cpu_next_word(self)); break;
        case 0x41: eor_indx(self, cpu_next_byte(self)); break;
        case 0x51: eor_indy(self, cpu_next_byte(self)); break;
        case 0x0A: asl_acc(self); break;
        case 0x06: asl_zp(self, cpu_next_byte(self)); break;
        case 0x16: asl_zpx(self, cpu_next_byte(self)); break;
        case 0x0E: asl_abs(self, cpu_next_word(self)); break;
        case 0x1E: asl_absx(self, cpu_next_word(self)); break;
        case 0x4A: lsr_acc(self); break;
        case 0x46: lsr_zp(self, cpu_next_byte(self)); break;
        case 0x56: lsr_zpx(self, cpu_next_byte(self)); break;
        case 0x4E: lsr_abs(self, cpu_next_word(self)); break;
        case 0x5E: lsr_absx(self, cpu_next_word(self)); break;
        case 0xE6: inc_zp(self, cpu_next_byte(self)); break;
        case 0xF6: inc_zpx(self, cpu_next_byte(self)); break;
        case 0xEE: inc_abs(self, cpu_next_word(self)); break;
        case 0xFE: inc_absx(self, cpu_next_word(self)); break;
        case 0xE8: inx(self); break;
        case 0xC8: iny(self); break;
        case 0xC6: dec_zp(self, cpu_next_byte(self)); break;
        case 0xD6: dec_zpx(self, cpu_next_byte(self)); break;
        case 0xCE: dec_abs(self, cpu_next_word(self)); break;
        case 0xDE: dec_absx(self, cpu_next_word(self)); break;
        case 0xCA: dex(self); break;
        case 0x88: dey(self); break;
        case 0x4C: jmp_abs(self, cpu_next_word(self)); break;
        case 0x6C: jmp_ind(self, cpu_next_word(self)); break;
        case 0x90: bcc_rel(self, cpu_next_byte(self)); break;
        case 0xB0: bcs_rel(self, cpu_next_byte(self)); break;
        case 0xF0: beq_rel(self, cpu_next_byte(self)); break;
        case 0xD0: bne_rel(self, cpu_next_byte(self)); break;
        case 0x10: bpl_rel(self, cpu_next_byte(self)); break;
        case 0x30: bmi_rel(self, cpu_next_byte(self)); break;
        case 0x50: bvc_rel(self, cpu_next_byte(self)); break;
        case 0x70: bvs_rel(self, cpu_next_byte(self)); break;
        case 0x18: clc(self); break;
        case 0x38: sec(self); break;
        case 0xD8: cld(self); break;
        case 0xF8: sed(self); break;
        case 0x58: cli(self); break;
        case 0x78: sei(self); break;
        case 0xB8: clv(self); break;
        case 0xC9: cmp_imm(self, cpu_next_byte(self)); break;
        case 0xC5: cmp_zp(self, cpu_next_byte(self)); break;
        case 0xD5: cmp_zpx(self, cpu_next_byte(self)); break;
        case 0xCD: cmp_abs(self, cpu_next_word(self)); break;
        case 0xDD: cmp_absx(self, cpu_next_word(self)); break;
        case 0xD9: cmp_absy(self, cpu_next_word(self)); break;
        case 0xC1: cmp_indx(self, cpu_next_byte(self)); break;
        case 0xD1: cmp_indy(self, cpu_next_byte(self)); break;
        case 0xE0: cpx_imm(self, cpu_next_byte(self)); break;
        case 0xE4: cpx_zp(self, cpu_next_byte(self)); break;
        case 0xEC: cpx_abs(self, cpu_next_word(self)); break;
        case 0xC0: cpy_imm(self, cpu_next_byte(self)); break;
        case 0xC4: cpy_zp(self, cpu_next_byte(self)); break;
        case 0xCC: cpy_abs(self, cpu_next_word(self)); break;
        case 0x48: pha(self); break;
        case 0x68: pla(self); break;
        case 0x28: plp(self); break;
        case 0x08: php(self); break;
        case 0x20: jsr(self); break;
        case 0x60: rts(self); break;
        case 0x40: rti(self); break;
        case 0x24: bit_zp(self, cpu_next_byte(self)); break;
        case 0x2C: bit_abs(self, cpu_next_word(self)); break;
        case 0x2A: rol_acc(self); break;
        case 0x26: rol_zp(self, cpu_next_byte(self)); break;
        case 0x36: rol_zpx(self, cpu_next_byte(self)); break;
        case 0x2E: rol_abs(self, cpu_next_word(self)); break;
        case 0x3E: rol_absx(self, cpu_next_word(self)); break;
        case 0x6A: ror_acc(self); break;
        case 0x66: ror_zp(self, cpu_next_byte(self)); break;
        case 0x76: ror_zpx(self, cpu_next_byte(self)); break;
        case 0x6E: ror_abs(self, cpu_next_word(self)); break;
        case 0x7E: ror_absx(self, cpu_next_word(self)); break;
        default:
            printf("Unsupported instruction: 0x%02X at PC: 0x%04X\n", opcode, self->pc - 1);
            exit(1); 
            break;
    }

    self->inst_cycles += INST_CYCLES[opcode];
    usize step_cycles = self->inst_cycles;
    self->total_cycles += step_cycles;
    self->inst_cycles = 0;

    return step_cycles;
}
