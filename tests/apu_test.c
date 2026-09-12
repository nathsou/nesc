#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "../src/lib/apu.h"

#define NTSC_CPU_FREQUENCY 1789773ULL

f32 apu_get_sample(APU* self);
u8 triangle_output(const APU_Triangle* triangle);

static void test_sample_clock_and_queue(void) {
    APU apu;
    const usize cycles = 100000;
    const usize expected_samples = (usize)((cycles * 44100ULL) / NTSC_CPU_FREQUENCY);

    apu_init(&apu, 44100);
    for (usize i = 0; i < cycles; i++) {
        apu_step(&apu);
    }

    assert(apu_buffered_samples(&apu) == expected_samples);

    f32* output = malloc((expected_samples + 1) * sizeof(*output));
    assert(output != NULL);
    apu_fill_buffer(&apu, output, expected_samples + 1);
    assert(apu_buffered_samples(&apu) == 0);
    assert(output[expected_samples] == output[expected_samples - 1]);
    assert(apu_take_underrun_samples(&apu) == 1);
    free(output);
}

static void test_maximum_mixer_index(void) {
    APU apu;
    apu_init(&apu, 44100);

    apu.triangle.enabled = true;
    apu.triangle.length_counter.counter = 1;
    apu.triangle.linear_counter = 1;
    apu.triangle.timer.period = 3;

    apu.noise.enabled = true;
    apu.noise.length_counter.counter = 1;
    apu.noise.shift_register = 0;
    apu.noise.envelope.constant_mode = true;
    apu.noise.envelope.constant_volume = 15;

    apu.dmc.output_level = 127;
    (void)apu_get_sample(&apu);
}

static void test_triangle_holds_output_when_halted(void) {
    APU apu;
    apu_init(&apu, 44100);

    apu.triangle.duty_cycle = 5;
    apu.triangle.enabled = false;
    apu.triangle.length_counter.counter = 0;
    apu.triangle.linear_counter = 0;
    apu.triangle.timer.period = 0;
    assert(triangle_output(&apu.triangle) == 10);
}

static void test_noise_length_lookup(void) {
    APU apu;
    apu_init(&apu, 44100);

    apu_write(&apu, 0x4015, 0x08);
    apu_write(&apu, 0x400C, 0x00);
    apu_write(&apu, 0x400F, 0xF8);
    assert(apu.noise.length_counter.counter == 30);

    apu_write(&apu, 0x400C, 0x20);
    for (usize i = 0; i < 40000; i++) {
        apu_step(&apu);
    }
    assert(apu.noise.length_counter.counter == 30);
}

static void test_frame_counter_modes(void) {
    APU apu;
    apu_init(&apu, 44100);

    apu_write(&apu, 0x4017, 0x00);
    for (usize i = 0; i < 29829; i++) {
        apu_step(&apu);
    }
    assert(apu_is_asserting_irq(&apu));
    assert((apu_read_status(&apu) & 0x40) != 0);
    assert(!apu_is_asserting_irq(&apu));

    apu_init(&apu, 44100);
    apu.triangle.counter_reload = 7;
    apu.triangle.linear_counter_reload = true;
    apu_write(&apu, 0x4017, 0x80);
    assert(apu.triangle.linear_counter == 7);
    for (usize i = 0; i < 37281; i++) {
        apu_step(&apu);
    }
    assert(!apu_is_asserting_irq(&apu));
}

int main(void) {
    puts("sample clock and queue");
    fflush(stdout);
    test_sample_clock_and_queue();
    puts("maximum mixer index");
    fflush(stdout);
    test_maximum_mixer_index();
    puts("triangle halt output");
    fflush(stdout);
    test_triangle_holds_output_when_halted();
    puts("noise length lookup");
    fflush(stdout);
    test_noise_length_lookup();
    puts("frame counter modes");
    fflush(stdout);
    test_frame_counter_modes();
    puts("APU tests passed");
    return 0;
}
