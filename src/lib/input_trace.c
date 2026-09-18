#include "input_trace.h"

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

static const u8 INPUT_TRACE_HEADER[] = { 'N', 'E', 'S', 'C', 'I', 'N', 'P', 1 };

static void input_trace_set_error(char* error, usize error_size, const char* format, ...) {
    if (error == NULL || error_size == 0) {
        return;
    }

    va_list args;
    va_start(args, format);
    vsnprintf(error, error_size, format, args);
    va_end(args);
}

bool input_trace_writer_open(InputTraceWriter* writer, const char* path, char* error, usize error_size) {
    writer->file = NULL;
    writer->frame_count = 0;

    FILE* file = fopen(path, "wb");
    if (file == NULL) {
        input_trace_set_error(error, error_size, "Could not open input trace '%s' for writing", path);
        return false;
    }

    if (fwrite(INPUT_TRACE_HEADER, 1, sizeof(INPUT_TRACE_HEADER), file) != sizeof(INPUT_TRACE_HEADER)) {
        input_trace_set_error(error, error_size, "Could not write input trace header to '%s'", path);
        fclose(file);
        return false;
    }

    writer->file = file;
    return true;
}

bool input_trace_writer_write_frame(InputTraceWriter* writer, u8 state, char* error, usize error_size) {
    if (writer->file == NULL) {
        input_trace_set_error(error, error_size, "Input trace is not open");
        return false;
    }

    if (fwrite(&state, 1, 1, writer->file) != 1) {
        input_trace_set_error(error, error_size, "Could not write input trace frame %zu", writer->frame_count);
        return false;
    }

    writer->frame_count++;
    if (writer->frame_count % 60 == 0 && fflush(writer->file) != 0) {
        input_trace_set_error(error, error_size, "Could not flush input trace after frame %zu", writer->frame_count);
        return false;
    }
    return true;
}

bool input_trace_writer_close(InputTraceWriter* writer, char* error, usize error_size) {
    bool ok = true;

    if (writer->file == NULL) {
        return true;
    }

    if (fflush(writer->file) != 0) {
        input_trace_set_error(error, error_size, "Could not flush input trace");
        ok = false;
    }

    if (fclose(writer->file) != 0) {
        input_trace_set_error(error, error_size, "Could not close input trace");
        ok = false;
    }

    writer->file = NULL;
    return ok;
}

bool input_trace_load(InputTrace* trace, const char* path, char* error, usize error_size) {
    trace->states = NULL;
    trace->frame_count = 0;

    FILE* file = fopen(path, "rb");
    if (file == NULL) {
        input_trace_set_error(error, error_size, "Could not open input trace '%s'", path);
        return false;
    }

    if (fseek(file, 0, SEEK_END) != 0) {
        input_trace_set_error(error, error_size, "Could not seek in input trace '%s'", path);
        fclose(file);
        return false;
    }

    long file_size = ftell(file);
    if (file_size < (long)sizeof(INPUT_TRACE_HEADER)) {
        input_trace_set_error(error, error_size, "Input trace '%s' is too short", path);
        fclose(file);
        return false;
    }

    if (fseek(file, 0, SEEK_SET) != 0) {
        input_trace_set_error(error, error_size, "Could not rewind input trace '%s'", path);
        fclose(file);
        return false;
    }

    u8 header[sizeof(INPUT_TRACE_HEADER)];
    if (fread(header, 1, sizeof(header), file) != sizeof(header) ||
        memcmp(header, INPUT_TRACE_HEADER, sizeof(INPUT_TRACE_HEADER)) != 0) {
        input_trace_set_error(error, error_size, "Input trace '%s' has an unsupported format", path);
        fclose(file);
        return false;
    }

    usize frame_count = (usize)(file_size - (long)sizeof(INPUT_TRACE_HEADER));
    if (frame_count == 0) {
        input_trace_set_error(error, error_size, "Input trace '%s' contains no frames", path);
        fclose(file);
        return false;
    }

    u8* states = (u8*)malloc(frame_count);
    if (states == NULL) {
        input_trace_set_error(error, error_size, "Could not allocate %zu input trace frames", frame_count);
        fclose(file);
        return false;
    }

    if (fread(states, 1, frame_count, file) != frame_count) {
        input_trace_set_error(error, error_size, "Could not read input trace frames from '%s'", path);
        free(states);
        fclose(file);
        return false;
    }

    fclose(file);
    trace->states = states;
    trace->frame_count = frame_count;
    return true;
}

void input_trace_free(InputTrace* trace) {
    free(trace->states);
    trace->states = NULL;
    trace->frame_count = 0;
}
