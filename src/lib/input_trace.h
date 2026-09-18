#ifndef NESC_INPUT_TRACE_H
#define NESC_INPUT_TRACE_H

#include "types.h"
#include <stdio.h>

typedef struct {
    FILE* file;
    usize frame_count;
} InputTraceWriter;

typedef struct {
    u8* states;
    usize frame_count;
} InputTrace;

bool input_trace_writer_open(InputTraceWriter* writer, const char* path, char* error, usize error_size);
bool input_trace_writer_write_frame(InputTraceWriter* writer, u8 state, char* error, usize error_size);
bool input_trace_writer_close(InputTraceWriter* writer, char* error, usize error_size);
bool input_trace_load(InputTrace* trace, const char* path, char* error, usize error_size);
void input_trace_free(InputTrace* trace);

#endif
