#ifndef SMB_APU_H
#define SMB_APU_H
#define AUDIO_BUFFER_SIZE (4 * 1024)

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include "types.h"
#include "cpu.h"

extern u8 audio_buffer[AUDIO_BUFFER_SIZE];
// reserve space for the web audio buffer
extern u8 web_audio_buffer[AUDIO_BUFFER_SIZE];
extern u16 audio_buffer_size;
extern u16 audio_buffer_index;

void apu_init(usize frequency);
void apu_write(u16 addr, u8 value);
void apu_step_frame(void);
void apu_fill_buffer(u8* cb_buffer, usize size);

#endif
