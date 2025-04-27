#ifndef SMB_INSTRUCTIONS_H
#define SMB_INSTRUCTIONS_H

#include "cpu.h"
#include "types.h"

void lda_imm(u8 value);
void lda_zp(u8 addr);
void lda_zpx(u8 addr);
void lda_zpy(u8 addr);
void lda_abs(u16 addr);
void lda_absx(u16 addr);
void lda_absy(u16 addr);
void lda_indx(u8 addr);
void lda_indy(u8 addr);

void ldx_imm(u8 value);
void ldx_zp(u8 addr);
void ldx_zpy(u8 addr);
void ldx_abs(u16 addr);
void ldx_absy(u16 addr);

void ldy_imm(u8 value);
void ldy_zp(u8 addr);
void ldy_zpx(u8 addr);
void ldy_abs(u16 addr);
void ldy_absx(u16 addr);

void sta_zp(u8 addr);
void sta_zpx(u8 addr);
void sta_zpy(u8 addr);
void sta_abs(u16 addr);
void sta_absx(u16 addr);
void sta_absy(u16 addr);
void sta_indx(u8 addr);
void sta_indy(u8 addr);

void stx_zp(u8 addr);
void stx_zpy(u8 addr);
void stx_abs(u16 addr);

void sty_zp(u8 addr);
void sty_zpx(u8 addr);
void sty_abs(u16 addr);

void adc_imm(u8 value);
void adc_zp(u8 addr);
void adc_zpx(u8 addr);
void adc_zpy(u8 addr);
void adc_abs(u16 addr);
void adc_absx(u16 addr);
void adc_absy(u16 addr);
void adc_indx(u8 addr);
void adc_indy(u8 addr);

void sbc_imm(u8 value);
void sbc_zp(u8 addr);
void sbc_zpx(u8 addr);
void sbc_abs(u16 addr);
void sbc_absx(u16 addr);
void sbc_absy(u16 addr);
void sbc_indx(u8 addr);
void sbc_indy(u8 addr);

void tax(void);
void tay(void);
void tsx(void);
void txa(void);
void txs(void);
void tya(void);

void and_imm(u8 value);
void and_zp(u8 addr);
void and_zpx(u8 addr);
void and_abs(u16 addr);
void and_absx(u16 addr);
void and_absy(u16 addr);
void and_indx(u8 addr);
void and_indy(u8 addr);

void ora_imm(u8 value);
void ora_zp(u8 addr);
void ora_zpx(u8 addr);
void ora_zpy(u8 addr);
void ora_abs(u16 addr);
void ora_absx(u16 addr);
void ora_absy(u16 addr);
void ora_indx(u8 addr);
void ora_indy(u8 addr);

void eor_imm(u8 value);
void eor_zp(u8 addr);
void eor_zpx(u8 addr);
void eor_abs(u16 addr);
void eor_absx(u16 addr);
void eor_absy(u16 addr);
void eor_indx(u8 addr);
void eor_indy(u8 addr);

void asl_acc(void);
void asl_abs(u16 addr);
void asl_zp(u8 addr);
void asl_zpx(u8 addr);
void asl_absx(u16 addr);

void lsr_acc(void);
void lsr_zp(u8 addr);
void lsr_abs(u16 addr);
void lsr_zpx(u8 addr);
void lsr_absx(u16 addr);

void inc_zp(u8 addr);
void inc_zpx(u8 addr);
void inc_abs(u16 addr);
void inc_absx(u16 addr);

void inx(void);
void iny(void);

void dec_zp(u8 addr);
void dec_zpx(u8 addr);
void dec_abs(u16 addr);
void dec_absx(u16 addr);

void dex(void);
void dey(void);

void clc(void);
void cld(void);
void cli(void);
void clv(void);

void sei(void);
void sec(void);
void sed(void);

void cmp_imm(u8 value);
void cmp_zp(u8 addr);
void cmp_zpx(u8 addr);
void cmp_zpy(u8 addr);
void cmp_abs(u16 addr);
void cmp_absx(u16 addr);
void cmp_absy(u16 addr);
void cmp_indx(u8 addr);
void cmp_indy(u8 addr);

void cpx_imm(u8 value);
void cpx_zp(u8 addr);
void cpx_abs(u16 addr);

void cpy_imm(u8 value);
void cpy_zp(u8 addr);
void cpy_abs(u16 addr);

void pha(void);
void pla(void);

void bit_zp(u8 addr);
void bit_abs(u16 addr);

void rol_acc(void);
void rol_zp(u8 addr);
void rol_zpx(u8 addr);
void rol_abs(u16 addr);
void rol_absx(u16 addr);

void ror_acc(void);
void ror_zp(u8 addr);
void ror_zpx(u8 addr);
void ror_abs(u16 addr);
void ror_absx(u16 addr);

void php(void);
void plp(void);
void brk(void);
void jsr(void);
void rts(void);
void rti(void);
void nmi(void);
void irq(void);

void jmp_abs(u16 addr);
void jmp_ind(u16 addr);
void bcc_rel(u8 offset);
void bcs_rel(u8 offset);
void beq_rel(u8 offset);
void bne_rel(u8 offset);
void bpl_rel(u8 offset);
void bmi_rel(u8 offset);
void bvc_rel(u8 offset);
void bvs_rel(u8 offset);

void nop(void);

#endif
