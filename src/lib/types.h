#ifndef NESC_TYPES_H
#define NESC_TYPES_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#define NESC_VERBOSE 1

#ifdef NESC_VERBOSE
#define LOG(...) printf(__VA_ARGS__)
#else
#define LOG(...)
#endif

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef size_t usize;
typedef int8_t s8;
typedef int16_t s16;
typedef int32_t s32;

#endif
