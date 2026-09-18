#define _POSIX_C_SOURCE 200809L

#include "input_trace.h"

#include <stdio.h>
#include <unistd.h>

int main(void) {
    char path[128];
    snprintf(path, sizeof(path), "/tmp/nesc-input-trace-test-%ld.bin", (long)getpid());

    const u8 expected[] = { 0x00, 0x81, 0x42, 0xff };
    char error[256];
    InputTraceWriter writer = {0};

    if (!input_trace_writer_open(&writer, path, error, sizeof(error))) {
        fprintf(stderr, "input trace writer open failed: %s\n", error);
        return 1;
    }

    for (usize i = 0; i < sizeof(expected); i++) {
        if (!input_trace_writer_write_frame(&writer, expected[i], error, sizeof(error))) {
            fprintf(stderr, "input trace writer write failed: %s\n", error);
            input_trace_writer_close(&writer, NULL, 0);
            remove(path);
            return 1;
        }
    }

    if (!input_trace_writer_close(&writer, error, sizeof(error))) {
        fprintf(stderr, "input trace writer close failed: %s\n", error);
        remove(path);
        return 1;
    }

    InputTrace trace = {0};
    if (!input_trace_load(&trace, path, error, sizeof(error))) {
        fprintf(stderr, "input trace load failed: %s\n", error);
        remove(path);
        return 1;
    }

    bool matches = trace.frame_count == sizeof(expected);
    for (usize i = 0; matches && i < sizeof(expected); i++) {
        matches = trace.states[i] == expected[i];
    }

    input_trace_free(&trace);
    remove(path);

    if (!matches) {
        fprintf(stderr, "input trace round trip changed frame states\n");
        return 1;
    }

    printf("input trace tests passed\n");
    return 0;
}
