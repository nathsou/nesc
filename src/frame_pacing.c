#include "frame_pacing.h"
#include <math.h>

static double clamp(double value, double low, double high) {
    return value < low ? low : value > high ? high : value;
}

void frame_pacing_init(FramePacing* pacing, double now, int display_hz, double audio_target) {
    // Match 60/120/180/240 Hz displays with a whole number of refreshes per frame.
    // Other refresh rates use native speed and necessarily repeat some images.
    double divisor = floor((double)display_hz / NES_NATIVE_FRAME_HZ + 0.5);
    double matched_hz = divisor >= 1.0 ? (double)display_hz / divisor : 0.0;
    double hz = fabs(matched_hz - NES_NATIVE_FRAME_HZ) < 0.5 ? matched_hz : NES_NATIVE_FRAME_HZ;
    *pacing = (FramePacing){
        .frame_hz = hz,
        .previous_time = now,
        .credit = 1.0,
        .filtered_queue = audio_target,
        .audio_pitch = hz / NES_NATIVE_FRAME_HZ,
    };
}

unsigned int frame_pacing_advance(FramePacing* pacing, double now) {
    double elapsed = fmax(0.0, now - pacing->previous_time);
    pacing->previous_time = now;
    pacing->credit += elapsed * pacing->frame_hz;
    // Round to the nearest presentation deadline: small vsync jitter must not
    // turn a regular 60 Hz sequence into alternating repeated/skipped frames.
    if (pacing->credit >= FRAME_PACING_MAX_CATCHUP + 0.5) {
        pacing->credit = FRAME_PACING_MAX_CATCHUP;
        pacing->resyncs++;
    }
    unsigned int frames = (unsigned int)fmax(0.0, floor(pacing->credit + 0.5));
    pacing->credit -= frames;
    return frames;
}

double frame_pacing_audio_pitch(FramePacing* pacing, double queued, double target, double elapsed) {
    // Correct clock drift slowly through resampling, never by hiding game frames.
    double dt = clamp(elapsed, 0.0, 0.1);
    pacing->filtered_queue += (queued - pacing->filtered_queue) * dt / (0.5 + dt);
    double correction = clamp((pacing->filtered_queue - target) / target * 0.01, -0.005, 0.005);
    double desired = pacing->frame_hz / NES_NATIVE_FRAME_HZ * (1.0 + correction);
    pacing->audio_pitch += (desired - pacing->audio_pitch) * dt / (1.0 + dt);
    return pacing->audio_pitch;
}
