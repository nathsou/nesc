#include "frame_pacing.h"
#include <assert.h>
#include <math.h>
#include <stdio.h>

static void test_display(int hz) {
    FramePacing pacing;
    frame_pacing_init(&pacing, 0.0, hz, 2048.0);
    unsigned int total = frame_pacing_advance(&pacing, 0.0);
    assert(total == 1);
    for (int i = 1; i <= hz * 120; i++) {
        double now = (double)i / hz + sin((double)i * 0.7) * 0.0007;
        unsigned int frames = frame_pacing_advance(&pacing, now);
        if (hz == 60) assert(frames == 1); // no jitter-induced duplicate or hidden frame
        if (hz >= 120) assert(frames <= 1);
        total += frames;
    }
    assert(fabs((double)total - (1.0 + 120.0 * pacing.frame_hz)) <= 1.0);
    assert(pacing.resyncs == 0);
}

static void test_stall(void) {
    FramePacing pacing;
    frame_pacing_init(&pacing, 0.0, 60, 2048.0);
    assert(frame_pacing_advance(&pacing, 0.0) == 1);
    assert(frame_pacing_advance(&pacing, 1.0 / 60.0) == 1);
    assert(frame_pacing_advance(&pacing, 4.0 / 60.0) == 3);
    assert(frame_pacing_advance(&pacing, 5.0) == FRAME_PACING_MAX_CATCHUP);
    assert(pacing.resyncs == 1);
    assert(frame_pacing_advance(&pacing, 5.0 + 1.0 / 60.0) == 1);
}

static void test_audio_clock(int hz, double device_rate) {
    FramePacing pacing;
    frame_pacing_init(&pacing, 0.0, hz, 2048.0);
    double queued = 2400.0;
    double output_credit = 0.0;
    double previous = 0.0;
    for (int i = 0; i < hz * 180; i++) {
        double now = (double)i / hz + sin((double)i * 0.7) * 0.0004;
        double dt = now - previous;
        previous = now;
        output_credit += dt * 44100.0 * device_rate;
        while (output_credit >= 512.0) {
            queued -= 512.0 * pacing.audio_pitch;
            output_credit -= 512.0;
            assert(queued > 0.0);
        }
        unsigned int frames = frame_pacing_advance(&pacing, now);
        queued += frames * 44100.0 / NES_NATIVE_FRAME_HZ;
        assert(queued < 4095.0);
        double pitch = frame_pacing_audio_pitch(&pacing, queued, 2048.0, dt);
        double base = pacing.frame_hz / NES_NATIVE_FRAME_HZ;
        assert(pitch >= base * 0.995 && pitch <= base * 1.005);
    }
    assert(pacing.resyncs == 0);
}

int main(void) {
    test_display(60);
    test_display(120);
    test_display(144);
    test_display(240);
    test_stall();
    for (int hz = 60; hz <= 120; hz += 60) {
        test_audio_clock(hz, 0.998);
        test_audio_clock(hz, 1.0);
        test_audio_clock(hz, 1.002);
    }
    test_audio_clock(144, 1.0);
    puts("frame pacing tests passed (refresh rates, jitter, stalls, audio clock drift)");
    return 0;
}
