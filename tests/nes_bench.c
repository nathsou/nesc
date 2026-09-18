#define _POSIX_C_SOURCE 200809L

#include "nes.h"
#include "input_trace.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <time.h>

#define DEFAULT_RUNS 5
#define MAX_RUNS 100

static bool monotonic_seconds(double* seconds) {
    struct timespec now;
    if (clock_gettime(CLOCK_MONOTONIC, &now) != 0) {
        return false;
    }

    *seconds = (double)now.tv_sec + (double)now.tv_nsec / 1000000000.0;
    return true;
}

static bool process_cpu_seconds(double* seconds) {
    struct rusage usage;
    if (getrusage(RUSAGE_SELF, &usage) != 0) {
        return false;
    }

    *seconds = (double)usage.ru_utime.tv_sec + (double)usage.ru_utime.tv_usec / 1000000.0
        + (double)usage.ru_stime.tv_sec + (double)usage.ru_stime.tv_usec / 1000000.0;
    return true;
}

static u64 framebuffer_hash(const PPU* ppu) {
    u64 hash = UINT64_C(14695981039346656037);

    for (usize i = 0; i < SCREEN_WIDTH * SCREEN_HEIGHT; i++) {
        u32 pixel = ppu->frame[i];
        for (usize component = 0; component < 3; component++) {
            hash ^= (pixel >> (component * 8)) & 0xff;
            hash *= UINT64_C(1099511628211);
        }
    }

    return hash;
}

static bool verify_frames = false;

static void replay_trace(NES* nes, const InputTrace* trace) {
    for (usize i = 0; i < trace->frame_count; i++) {
        cpu_update_controller1(&nes->cpu, trace->states[i]);
        nes_step_frame(nes);
        if (verify_frames) {
            printf("%zu %016llX\n", i, (unsigned long long)framebuffer_hash(&nes->ppu));
        }
    }

}

static bool execute_trace(const char* rom_path, const InputTrace* trace, bool timed,
                          double* elapsed_seconds, double* cpu_seconds,
                          u64* final_frame_hash,
                          char* error, usize error_size) {
    NES nes = {0};
    Result init_result = nes_init_from_file(&nes, rom_path, 44100);
    if (!init_result.ok) {
        snprintf(error, error_size, "Could not initialize ROM: %s", init_result.error);
        return false;
    }

    double start = 0.0;
    double end = 0.0;
    double cpu_start = 0.0;
    double cpu_end = 0.0;
    if (timed) {
        if (!monotonic_seconds(&start)) {
            snprintf(error, error_size, "Could not read monotonic clock");
            nes_free(&nes);
            return false;
        }
        if (!process_cpu_seconds(&cpu_start)) {
            snprintf(error, error_size, "Could not read process CPU time");
            nes_free(&nes);
            return false;
        }
    }

#ifdef NESC_COMPONENT_PROFILE
    if (timed && !nes_component_profile_start()) {
        snprintf(error, error_size, "Could not start component sampling timer");
        nes_free(&nes);
        return false;
    }
#endif

    replay_trace(&nes, trace);

#ifdef NESC_COMPONENT_PROFILE
    if (timed) {
        nes_component_profile_stop();
    }
#endif

    if (timed) {
        if (!process_cpu_seconds(&cpu_end)) {
            snprintf(error, error_size, "Could not read process CPU time");
            nes_free(&nes);
            return false;
        }
        if (!monotonic_seconds(&end)) {
            snprintf(error, error_size, "Could not read monotonic clock");
            nes_free(&nes);
            return false;
        }
    }

    if (timed) {
        *elapsed_seconds = end - start;
        *cpu_seconds = cpu_end - cpu_start;
    }
    *final_frame_hash = framebuffer_hash(&nes.ppu);
    nes_free(&nes);
    return true;
}

static int compare_seconds(const void* lhs, const void* rhs) {
    double a = *(const double*)lhs;
    double b = *(const double*)rhs;
    return (a > b) - (a < b);
}

static bool parse_run_count(const char* argument, usize* runs) {
    errno = 0;
    char* end = NULL;
    unsigned long long value = strtoull(argument, &end, 10);
    if (errno != 0 || end == argument || *end != '\0' || value == 0 || value > MAX_RUNS) {
        return false;
    }

    *runs = (usize)value;
    return true;
}

int main(int argc, char* argv[]) {
    if (argc < 3 || argc > 4) {
        fprintf(stderr, "Usage: %s <rom_path> <input_trace> [runs: 1-%d, default %d | --verify]\n",
                argv[0], MAX_RUNS, DEFAULT_RUNS);
        return 2;
    }

    usize runs = DEFAULT_RUNS;
    verify_frames = argc == 4 && strcmp(argv[3], "--verify") == 0;
    if (argc == 4 && !verify_frames && !parse_run_count(argv[3], &runs)) {
        fprintf(stderr, "Invalid run count '%s' (expected 1-%d)\n", argv[3], MAX_RUNS);
        return 2;
    }

    InputTrace trace = {0};
    char error[256];
    if (!input_trace_load(&trace, argv[2], error, sizeof(error))) {
        fprintf(stderr, "%s\n", error);
        return 2;
    }

    printf("Input trace: %zu emulated frames\n", trace.frame_count);
    printf("Warm-up: replaying once (not timed)\n");

    u64 expected_hash = 0;
    if (!execute_trace(argv[1], &trace, false, NULL, NULL, &expected_hash, error, sizeof(error))) {
        fprintf(stderr, "%s\n", error);
        input_trace_free(&trace);
        return 2;
    }

    if (verify_frames) {
        input_trace_free(&trace);
        return 0;
    }

    double* samples = (double*)malloc(runs * sizeof(double));
    double* cpu_samples = (double*)malloc(runs * sizeof(double));
    if (samples == NULL || cpu_samples == NULL) {
        fprintf(stderr, "Could not allocate benchmark samples\n");
        free(cpu_samples);
        free(samples);
        input_trace_free(&trace);
        return 2;
    }

    for (usize run = 0; run < runs; run++) {
        u64 run_hash = 0;
#ifdef NESC_COMPONENT_PROFILE
        nes_component_profile_reset();
#endif
        if (!execute_trace(argv[1], &trace, true, &samples[run], &cpu_samples[run],
                           &run_hash, error, sizeof(error))) {
            fprintf(stderr, "%s\n", error);
            free(cpu_samples);
            free(samples);
            input_trace_free(&trace);
            return 2;
        }

        if (run_hash != expected_hash) {
            fprintf(stderr, "Replay output changed between runs (framebuffer hash mismatch)\n");
            free(cpu_samples);
            free(samples);
            input_trace_free(&trace);
            return 1;
        }

        double frames_per_second = (double)trace.frame_count / samples[run];
        double cpu_frames_per_second = (double)trace.frame_count / cpu_samples[run];
        printf("Run %zu: wall %.6f s, %.1f frames/s; CPU %.6f s, %.1f frames/s\n",
               run + 1, samples[run], frames_per_second,
               cpu_samples[run], cpu_frames_per_second);

#ifdef NESC_COMPONENT_PROFILE
        NESComponentProfile profile = nes_component_profile_read();
        u64 total_profile_samples = 0;
        for (usize i = 0; i < NES_PROFILE_PHASE_COUNT; i++) {
            total_profile_samples += profile.phase_samples[i];
        }
        if (total_profile_samples == 0) {
            fprintf(stderr, "Component profiler collected no samples\n");
            free(cpu_samples);
            free(samples);
            input_trace_free(&trace);
            return 1;
        }

        double cpu_share = (double)profile.phase_samples[NES_PROFILE_CPU] / (double)total_profile_samples;
        double apu_share = (double)profile.phase_samples[NES_PROFILE_APU] / (double)total_profile_samples;
        double ppu_share = (double)profile.phase_samples[NES_PROFILE_PPU] / (double)total_profile_samples;
        double other_share = (double)profile.phase_samples[NES_PROFILE_OTHER] / (double)total_profile_samples;
        printf("  Sampled CPU share: CPU %.1f%% (%.3f s), PPU %.1f%% (%.3f s), "
               "APU %.1f%% (%.3f s), scheduler/other %.1f%% (%.3f s); %llu samples\n",
               cpu_share * 100.0, cpu_share * cpu_samples[run],
               ppu_share * 100.0, ppu_share * cpu_samples[run],
               apu_share * 100.0, apu_share * cpu_samples[run],
               other_share * 100.0, other_share * cpu_samples[run],
               (unsigned long long)total_profile_samples);
#endif
    }

    qsort(samples, runs, sizeof(double), compare_seconds);
    qsort(cpu_samples, runs, sizeof(double), compare_seconds);
    double median_seconds = runs & 1
        ? samples[runs / 2]
        : (samples[runs / 2 - 1] + samples[runs / 2]) / 2.0;
    double median_frames_per_second = (double)trace.frame_count / median_seconds;
    double median_cpu_seconds = runs & 1
        ? cpu_samples[runs / 2]
        : (cpu_samples[runs / 2 - 1] + cpu_samples[runs / 2]) / 2.0;
    double median_cpu_frames_per_second = (double)trace.frame_count / median_cpu_seconds;

    printf("Median wall: %.6f s, %.1f frames/s (%.2fx 60 fps)\n",
           median_seconds, median_frames_per_second, median_frames_per_second / 60.0);
    printf("Median CPU: %.6f s, %.1f frames/s (%.2fx 60 fps)\n",
           median_cpu_seconds, median_cpu_frames_per_second, median_cpu_frames_per_second / 60.0);
    printf("Final framebuffer FNV-1a: %016llX\n", (unsigned long long)expected_hash);

    free(cpu_samples);
    free(samples);
    input_trace_free(&trace);
    return 0;
}
