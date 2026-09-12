#ifndef NESC_APU_H
#define NESC_APU_H
#define AUDIO_BUFFER_SIZE (4 * 1024)

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdatomic.h>
#include <string.h>
#include "types.h"

typedef struct {
    bool enabled;
    u8 counter;
} APU_LengthCounter;

typedef struct {
    u16 counter;
    u16 period;
} APU_Timer;

typedef struct {
    bool constant_mode;
    bool looping;
    bool start;
    u8 constant_volume;
    u8 period;
    u8 divider;
    u8 decay;
} APU_Envelope;

typedef struct {
    bool enabled;
    u8 duty_mode;
    u8 duty_cycle;
    APU_Timer timer;
    APU_LengthCounter length_counter;
    APU_Envelope envelope;
    bool sweep_enabled;
    u8 sweep_period;
    bool sweep_negate;
    u8 sweep_shift;
    bool sweep_reload;
    u8 sweep_divider;
    bool sweep_mute;
} APU_Pulse;

typedef struct {
    bool enabled;
    bool control_flag;
    u8 counter_reload;
    APU_Timer timer;
    APU_LengthCounter length_counter;
    u8 linear_counter;
    bool linear_counter_reload;
    u8 duty_cycle;
} APU_Triangle;

typedef struct {
    bool enabled;
    APU_LengthCounter length_counter;
    APU_Envelope envelope;
    APU_Timer timer;
    u16 shift_register;
    bool mode;
} APU_Noise;

typedef struct {
    bool enabled;
    bool interrupt_flag;
    bool loop_flag;
    APU_Timer timer;
    u8 output_level;
    u16 sample_addr;
    u16 sample_len;
    u16 current_addr;
    u16 bytes_remaining;
    u8 shift_register;
    bool silence_flag;
    u8 output_bits_remaining;
    bool irq_enabled;
    u16 memory_read_request;
    bool has_memory_request;
    usize cpu_stall_cycles;
} APU_DeltaModulationChannel;

typedef struct {
    float b0;
    float b1;
    float a1;
    float prev_x;
    float prev_y;
} APU_Filter;

typedef struct {
    usize sample_rate;
    u64 cycle;
    u64 sample_phase;
    bool five_step_mode;
    bool irq_inhibit;
    bool frame_interrupt;
    f32 audio_buffer[AUDIO_BUFFER_SIZE];
    _Atomic usize audio_read_index;
    _Atomic usize audio_write_index;
    f32 audio_last_sample;
    _Atomic usize audio_underrun_samples;
    APU_Pulse pulse1, pulse2;
    APU_Triangle triangle;
    APU_Noise noise;
    APU_DeltaModulationChannel dmc;
    APU_Filter filter1, filter2, filter3;
    usize frame_counter;
} APU;

void apu_init(APU* self, usize frequency);
void apu_write(APU* self, u16 addr, u8 value);
void apu_step(APU* self);
void apu_fill_buffer(APU* self, f32* cb_buffer, usize size);
usize apu_buffered_samples(const APU* self);
usize apu_take_underrun_samples(APU* self);
u8 apu_read_status(APU* self);
bool apu_is_asserting_irq(APU* self);

#endif
