#ifndef NESC_FRAME_PACING_H
#define NESC_FRAME_PACING_H

// Matches the core's NTSC CPU clock and average PPU frame length.
#define NES_NATIVE_FRAME_HZ (1789773.0 * 3.0 / (341.0 * 262.0 - 0.5))
#define FRAME_PACING_MAX_CATCHUP 4

typedef struct {
    double frame_hz;
    double previous_time;
    double credit;
    double filtered_queue;
    double audio_pitch;
    unsigned int resyncs;
} FramePacing;

void frame_pacing_init(FramePacing* pacing, double now, int display_hz, double audio_target);
unsigned int frame_pacing_advance(FramePacing* pacing, double now);
double frame_pacing_audio_pitch(FramePacing* pacing, double queued, double target, double elapsed);

#endif
