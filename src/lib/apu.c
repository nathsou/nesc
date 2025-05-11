#include "apu.h"
#define PI 3.141592653f

#define CPU_FREQUENCY 1789773
#define FRAME_RATE 60

// Length Counter

const u8 LENGTH_LOOKUP[] = {
    10, 254, 20, 2, 40, 4, 80, 6,
    160, 8, 60, 10, 14, 12, 26, 14,
    12, 16, 24, 18, 48, 20, 96, 22,
    192, 24, 72, 26, 16, 28, 32, 30,
};

void length_counter_init(APU_LengthCounter* lc) {
    lc->enabled = false;
    lc->counter = 0;
}

inline void length_counter_reset_to_zero(APU_LengthCounter* lc) {
    lc->counter = 0;
}

inline void length_counter_step(APU_LengthCounter* lc) {
    if (lc->enabled && lc->counter > 0) {
        lc->counter--;
    }
}

inline void length_counter_set(APU_LengthCounter* lc, u8 val) {
    lc->counter = LENGTH_LOOKUP[val];
}

// Set enabled status
inline void length_counter_set_enabled(APU_LengthCounter* lc, bool enabled) {
    lc->enabled = enabled;
}

// Check if counter is zero
inline bool length_counter_is_zero(const APU_LengthCounter* lc) {
    return lc->counter == 0;
}

// Timer

void timer_init(APU_Timer* self) {
    self->counter = 0;
    self->period = 0;
}

inline bool timer_step(APU_Timer* self) {
    if (self->counter == 0) {
        self->counter = self->period;
        return true;
    } else {
        self->counter -= 1;
        return false;
    }
}

// Envelope

void envelope_init(APU_Envelope* self) {
    self->constant_mode = false;
    self->looping = false;
    self->start = false;
    self->constant_volume = 0;
    self->period = 0;
    self->divider = 0;
    self->decay = 0;
}

void envelope_step(APU_Envelope* self) {
    if (self->start) {
        self->start = false;
        self->decay = 15;
        self->divider = self->period;
    } else if (self->divider == 0) {
        if (self->decay > 0) {
            self->decay -= 1;
        } else if (self->looping) {
            self->decay = 15;
        }
        self->divider = self->period;
    } else {
        self->divider -= 1;
    }
}

inline u8 envelope_output(const APU_Envelope* self) {
    return self->constant_mode ? self->constant_volume : self->decay;
}

// Pulse

static const float PULSE_MIXER_LOOKUP[] = {
    0.0f, 0.011609139f, 0.02293948f, 0.034000948f,
    0.044803f, 0.05535466f, 0.06566453f, 0.07574082f,
    0.0855914f, 0.09522375f, 0.10464504f, 0.11386215f,
    0.12288164f, 0.1317098f, 0.14035264f, 0.14881596f,
    0.15710525f, 0.16522588f, 0.17318292f, 0.18098126f,
    0.18862559f, 0.19612046f, 0.20347017f, 0.21067894f,
    0.21775076f, 0.2246895f, 0.23149887f, 0.23818247f,
    0.24474378f, 0.25118607f, 0.25751257f, 0.26372638f,
};

static const u8 PULSE_DUTY_TABLE[][8] = {
    { 0, 1, 0, 0, 0, 0, 0, 0 },
    { 0, 1, 1, 0, 0, 0, 0, 0 },
    { 0, 1, 1, 1, 1, 0, 0, 0 },
    { 1, 0, 0, 1, 1, 1, 1, 1 },
};

void pulse_init(APU_Pulse* self) {
    self->enabled = false;
    self->duty_mode = 0;
    self->duty_cycle = 0;
    timer_init(&self->timer);
    length_counter_init(&self->length_counter);
    envelope_init(&self->envelope);
    self->sweep_enabled = false;
    self->sweep_period = 0;
    self->sweep_negate = false;
    self->sweep_shift = 0;
    self->sweep_reload = false;
    self->sweep_divider = 0;
    self->sweep_mute = false;
}

void pulse_set_enabled(APU_Pulse* self, bool enabled) {
    self->enabled = enabled;

    if (!enabled) {
        length_counter_reset_to_zero(&self->length_counter);
    }
}

void pulse_step_timer(APU_Pulse* self) {
    if (timer_step(&self->timer)) {
        self->duty_cycle = (self->duty_cycle + 1) & 7;
    }
}

inline void pulse_step_length_counter(APU_Pulse* self) {
    length_counter_step(&self->length_counter);
}

inline void pulse_step_envelope(APU_Pulse* self) {
    envelope_step(&self->envelope);
}

void pulse_write_control(APU_Pulse* self, u8 value) {
    self->duty_mode = (value >> 6) & 0b11;
    bool halt_length_counter = (value & 0b00100000) != 0;
    length_counter_set_enabled(&self->length_counter, !halt_length_counter);
    self->envelope.looping = halt_length_counter;
    self->envelope.constant_mode = (value & 0b00010000) != 0;
    self->envelope.period = value & 0b1111;
    self->envelope.constant_volume = value & 0b1111;
    self->envelope.start = true;
}

void pulse_write_reload_low(APU_Pulse* self, u8 value) {
    self->timer.period = (u16)((self->timer.period & 0xff00) | ((u16)value));
}

void pulse_write_reload_high(APU_Pulse* self, u8 value) {
    self->timer.period = (u16)((self->timer.period & 0x00ff) | (((u16)(value & 7)) << 8));
    self->duty_cycle = 0;
    self->envelope.start = true;
    length_counter_set(&self->length_counter, value >> 3);
}

void pulse_write_sweep(APU_Pulse* self, u8 value) {
    self->sweep_enabled = (value & 0b10000000) != 0;
    self->sweep_period = (value >> 4) & 0b111;
    self->sweep_negate = (value & 0b1000) != 0;
    self->sweep_shift = value & 0b111;
    self->sweep_reload = true;
}

u16 pulse_sweep_target_period(APU_Pulse* self) {
    u16 change_amount = self->timer.period >> self->sweep_shift;

    if (self->sweep_negate) {
        if (change_amount > self->timer.period) {
            return 0;
        } else {
            return self->timer.period - change_amount;
        }
    } else {
        return self->timer.period + change_amount;
    }
}

void pulse_step_sweep(APU_Pulse* self) {
    u16 target_period = pulse_sweep_target_period(self);
    self->sweep_mute = self->timer.period < 8 || target_period > 0x7ff;

    if (self->sweep_divider == 0 && self->sweep_enabled && !self->sweep_mute) {
        self->timer.period = target_period;
    }

    if (self->sweep_divider == 0 || self->sweep_reload) {
        self->sweep_divider = self->sweep_period;
        self->sweep_reload = false;
    } else {
        self->sweep_divider--;
    }
}

u8 pulse_output(APU_Pulse* self) {
    if (
        !self->enabled ||
        self->sweep_mute ||
        length_counter_is_zero(&self->length_counter) ||
        PULSE_DUTY_TABLE[self->duty_mode][self->duty_cycle] == 0
    ) {
        return 0;
    }

    return envelope_output(&self->envelope);
}

// Triangle

static const u8 SEQUENCER_LOOKUP[32] = {
    15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0,
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
};

static const float TRIANGLE_MIXER_LOOKUP[] = {
    0.0f, 0.006699824f, 0.01334502f, 0.019936256f, 0.02647418f, 0.032959443f, 0.039392676f, 0.0457745f,
    0.052105535f, 0.05838638f, 0.064617634f, 0.07079987f, 0.07693369f, 0.08301962f, 0.08905826f, 0.095050134f,
    0.100995794f, 0.10689577f, 0.11275058f, 0.118560754f, 0.12432679f, 0.13004918f, 0.13572845f, 0.14136505f,
    0.1469595f, 0.15251222f, 0.1580237f, 0.1634944f, 0.16892476f, 0.17431524f, 0.17966628f, 0.1849783f,
    0.19025174f, 0.19548698f, 0.20068447f, 0.20584463f, 0.21096781f, 0.21605444f, 0.22110492f, 0.2261196f,
    0.23109888f, 0.23604311f, 0.24095272f, 0.245828f, 0.25066936f, 0.2554771f, 0.26025164f, 0.26499328f,
    0.26970237f, 0.27437922f, 0.27902418f, 0.28363758f, 0.28821972f, 0.29277095f, 0.29729152f, 0.3017818f,
    0.3062421f, 0.31067267f, 0.31507385f, 0.31944588f, 0.32378912f, 0.32810378f, 0.3323902f, 0.3366486f,
    0.3408793f, 0.34508255f, 0.34925863f, 0.35340777f, 0.35753027f, 0.36162636f, 0.36569634f, 0.36974037f,
    0.37375876f, 0.37775174f, 0.38171956f, 0.38566244f, 0.38958064f, 0.39347437f, 0.39734384f, 0.4011893f,
    0.405011f, 0.40880907f, 0.41258383f, 0.41633546f, 0.42006415f, 0.42377013f, 0.4274536f, 0.43111476f,
    0.43475384f, 0.43837097f, 0.44196644f, 0.4455404f, 0.449093f, 0.45262453f, 0.45613506f, 0.4596249f,
    0.46309412f, 0.46654293f, 0.46997157f, 0.47338015f, 0.47676894f, 0.48013794f, 0.48348752f, 0.4868177f,
    0.49012873f, 0.4934207f, 0.49669388f, 0.49994832f, 0.50318426f, 0.50640184f, 0.5096012f, 0.51278245f,
    0.51594585f, 0.5190914f, 0.5222195f, 0.52533007f, 0.52842325f, 0.5314993f, 0.53455836f, 0.5376005f,
    0.54062593f, 0.5436348f, 0.54662704f, 0.54960304f, 0.55256283f, 0.55550647f, 0.5584343f, 0.56134623f,
    0.5642425f, 0.56712323f, 0.5699885f, 0.5728384f, 0.5756732f, 0.57849294f, 0.5812977f, 0.5840876f,
    0.5868628f, 0.58962345f, 0.59236956f, 0.59510136f, 0.5978189f, 0.6005223f, 0.6032116f, 0.605887f,
    0.60854864f, 0.6111966f, 0.6138308f, 0.61645156f, 0.619059f, 0.62165314f, 0.624234f, 0.62680185f,
    0.6293567f, 0.63189864f, 0.6344277f, 0.6369442f, 0.63944805f, 0.64193934f, 0.64441824f, 0.64688486f,
    0.6493392f, 0.6517814f, 0.6542115f, 0.65662974f, 0.65903604f, 0.6614306f, 0.6638134f, 0.66618466f,
    0.66854435f, 0.6708926f, 0.67322946f, 0.67555505f, 0.67786944f, 0.68017274f, 0.68246496f, 0.6847462f,
    0.6870166f, 0.6892762f, 0.69152504f, 0.6937633f, 0.6959909f, 0.69820803f, 0.7004148f, 0.7026111f,
    0.7047972f, 0.7069731f, 0.7091388f, 0.7112945f, 0.7134401f, 0.7155759f, 0.7177018f, 0.7198179f,
    0.72192425f, 0.72402096f, 0.726108f, 0.72818565f, 0.7302538f, 0.73231256f, 0.73436195f, 0.7364021f,
};

void triangle_init(APU_Triangle* tc) {
    tc->enabled = false;
    tc->control_flag = false;
    tc->counter_reload = 0;
    timer_init(&tc->timer);
    length_counter_init(&tc->length_counter);
    tc->linear_counter = 0;
    tc->linear_counter_reload = false;
    tc->duty_cycle = 0;
}

void triangle_write_setup(APU_Triangle* tc, u8 val) {
    tc->control_flag = (val & 0x80) != 0;
    tc->counter_reload = val & 0x7F;
}

void triangle_write_timer_low(APU_Triangle* tc, u8 val) {
    tc->timer.period = (tc->timer.period & 0xFF00) | val;
}

void triangle_write_timer_high(APU_Triangle* tc, u8 val) {
    tc->timer.period = (tc->timer.period & 0x00FF) | (u16)((u16)(val & 0x07) << 8);
    tc->timer.counter = tc->timer.period;
    length_counter_set(&tc->length_counter, val >> 3);
    tc->linear_counter_reload = true;
}

void triangle_step_linear_counter(APU_Triangle* tc) {
    if (tc->linear_counter_reload) {
        tc->linear_counter = tc->counter_reload;
    } else if (tc->linear_counter > 0) {
        tc->linear_counter--;
    }

    if (!tc->control_flag) {
        tc->linear_counter_reload = false;
    }
}

inline void triangle_step_length_counter(APU_Triangle* tc) {
    length_counter_step(&tc->length_counter);
}

void triangle_step_timer(APU_Triangle* tc) {
    if (timer_step(&tc->timer) && tc->linear_counter > 0 && !length_counter_is_zero(&tc->length_counter)) {
        tc->duty_cycle = (tc->duty_cycle + 1) & 31;
    }
}

void triangle_set_enabled(APU_Triangle* tc, bool enabled) {
    tc->enabled = enabled;
    if (!enabled) {
        length_counter_reset_to_zero(&tc->length_counter);
    }
}

u8 triangle_output(const APU_Triangle* tc) {
    if (!tc->enabled ||
        length_counter_is_zero(&tc->length_counter) ||
        tc->linear_counter == 0 ||
        tc->timer.period <= 2) {
        return 0;
    }
    return SEQUENCER_LOOKUP[tc->duty_cycle & 0x1F];
}

// Noise

static const u16 NOISE_PERIOD_TABLE[16] = {
    4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068
};

void noise_init(APU_Noise* self) {
    self->enabled = false;
    self->length_counter.counter = 0;
    self->envelope.constant_mode = false;
    self->envelope.looping = false;
    self->envelope.start = false;
    self->envelope.constant_volume = 0;
    self->envelope.period = 0;
    self->envelope.divider = 0;
    self->envelope.decay = 0;
    self->timer.counter = 0;
    self->timer.period = 0;
    self->shift_register = 1;
    self->mode = false;
}

void noise_set_enabled(APU_Noise* self, bool enabled) {
    self->enabled = enabled;
    if (!enabled) {
        self->length_counter.counter = 0;
    }
}

void noise_step_timer(APU_Noise* self) {
    if (timer_step(&self->timer)) {
        u8 bit = self->mode ? 6 : 1;
        u16 bit0 = self->shift_register & 1;
        u16 other_bit = (self->shift_register >> bit) & 1;
        u16 feedback = bit0 ^ other_bit;
        self->shift_register >>= 1;
        self->shift_register |= feedback << 14;
    }
}

void noise_step_length_counter(APU_Noise* self) {
    if (self->length_counter.counter > 0) {
        self->length_counter.counter--;
    }
}

void noise_step_envelope(APU_Noise* self) {
    envelope_step(&self->envelope);
}

void noise_write_control(APU_Noise* self, u8 val) {
    bool halt_length_counter = (val & 0x20) != 0;
    self->length_counter.counter = halt_length_counter ? 0 : self->length_counter.counter;
    self->envelope.looping = halt_length_counter;
    self->envelope.constant_mode = (val & 0x10) != 0;
    self->envelope.period = val & 0x0F;
    self->envelope.constant_volume = val & 0x0F;
}

void noise_write_period(APU_Noise* self, u8 val) {
    self->mode = (val & 0x80) != 0;
    self->timer.period = NOISE_PERIOD_TABLE[val & 0x0F];
}

void noise_write_length(APU_Noise* self, u8 val) {
    self->length_counter.counter = val >> 3;
    self->envelope.start = true;
}

u8 noise_output(const APU_Noise* self) {
    if ((self->shift_register & 1) == 1 || self->length_counter.counter == 0) {
        return 0;
    } else {
        return envelope_output(&self->envelope);
    }
}

// DMC

static const u16 DELTA_MODULATION_RATES[] = {
    428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54,
};

void dmc_init(APU_DeltaModulationChannel* dmc) {
    dmc->enabled = false;
    dmc->interrupt_flag = false;
    dmc->loop_flag = false;
    timer_init(&dmc->timer);
    dmc->output_level = 0;
    dmc->sample_addr = 0;
    dmc->sample_len = 0;
    dmc->current_addr = 0;
    dmc->bytes_remaining = 0;
    dmc->shift_register = 0;
    dmc->silence_flag = false;
    dmc->output_bits_remaining = 0;
    dmc->irq_enabled = false;
    dmc->has_memory_request = false;
    dmc->cpu_stall_cycles = 0;
}

void dmc_restart(APU_DeltaModulationChannel* dmc) {
    dmc->current_addr = dmc->sample_addr;
    dmc->bytes_remaining = dmc->sample_len;
}

void dmc_step_shifter(APU_DeltaModulationChannel* dmc) {
    if (dmc->output_bits_remaining != 0) {
        if (!dmc->silence_flag) {
            if (dmc->shift_register & 1) {
                if (dmc->output_level <= 125) {
                    dmc->output_level += 2;
                }
            } else {
                if (dmc->output_level >= 2) {
                    dmc->output_level -= 2;
                }
            }
        }
        dmc->shift_register >>= 1;
        dmc->output_bits_remaining--;
    }
}

void dmc_step_reader(APU_DeltaModulationChannel* dmc) {
    if (dmc->output_bits_remaining == 0 && dmc->bytes_remaining > 0) {
        dmc->cpu_stall_cycles += 4;
        dmc->memory_read_request = dmc->current_addr;
        dmc->has_memory_request = true;
        dmc->output_bits_remaining = 8;

        dmc->current_addr = (dmc->current_addr == 0xFFFF) ? 0x8000 : dmc->current_addr + 1;
        dmc->bytes_remaining--;

        if (dmc->bytes_remaining == 0) {
            if (dmc->loop_flag) {
                dmc_restart(dmc);
            } else if (dmc->irq_enabled) {
                dmc->interrupt_flag = true;
            }
        }
    }
}

void dmc_step_timer(APU_DeltaModulationChannel* dmc) {
    if (dmc->enabled) {
        dmc_step_reader(dmc);

        if (!dmc->has_memory_request && timer_step(&dmc->timer)) {
            dmc_step_shifter(dmc);
        }
    }
}

void dmc_write_control(APU_DeltaModulationChannel* dmc, u8 val) {
    dmc->irq_enabled = (val & 0x80) != 0;
    dmc->loop_flag = (val & 0x40) != 0;
    dmc->timer.period = DELTA_MODULATION_RATES[val & 0x0F];
}

inline void dmc_write_output(APU_DeltaModulationChannel* dmc, u8 val) {
    dmc->output_level = val & 0x7F;
}

inline void dmc_write_sample_addr(APU_DeltaModulationChannel* dmc, u8 val) {
    dmc->sample_addr = 0xC000 | (u16)((u16)val << 6);
}

inline void dmc_write_sample_len(APU_DeltaModulationChannel* dmc, u8 val) {
    dmc->sample_len = (u16)((u16)val << 4) | 1;
}

inline void dmc_set_memory_read_response(APU_DeltaModulationChannel* dmc, u8 val) {
    dmc->shift_register = val;
    dmc->has_memory_request = false;
    if (timer_step(&dmc->timer)) {
        dmc_step_shifter(dmc);
    }
}

inline bool dmc_is_active(const APU_DeltaModulationChannel* dmc) {
    return dmc->bytes_remaining > 0;
}

inline void dmc_clear_interrupt_flag(APU_DeltaModulationChannel* dmc) {
    dmc->interrupt_flag = false;
}

void dmc_set_enabled(APU_DeltaModulationChannel* dmc, bool enabled) {
    dmc->enabled = enabled;
    if (!enabled) {
        dmc->bytes_remaining = 0;
    } else if (dmc->bytes_remaining == 0) {
        dmc_restart(dmc);
    }
}

inline u8 dmc_output(const APU_DeltaModulationChannel* dmc) {
    return dmc->output_level;
}

// APU_Filters

void filter_init_low_pass(APU_Filter* f, usize sample_rate, float cutoff) {
    float c = (float)sample_rate / (cutoff * PI);
    float a0 = 1.0f / (1.0f + c);

    f->b0 = a0;
    f->b1 = a0;
    f->a1 = (1.0f - c) * a0;
    f->prev_x = 0.0f;
    f->prev_y = 0.0f;
}

void filter_init_high_pass(APU_Filter* f, usize sample_rate, float cutoff) {
    float c = (float)sample_rate / (cutoff * PI);
    float a0 = 1.0f / (1.0f + c);

    f->b0 = c * a0;
    f->b1 = -c * a0;
    f->a1 = (1.0f - c) * a0;
    f->prev_x = 0.0f;
    f->prev_y = 0.0f;
}

float filter_output(APU_Filter* f, float x) {
    float y = f->b0 * x + f->b1 * f->prev_x - f->a1 * f->prev_y;
    f->prev_x = x;
    f->prev_y = y;

    return y;
}

// APU

void apu_init(APU* self, usize frequency) {
    memset(self->audio_buffer, 0, AUDIO_BUFFER_SIZE);
    pulse_init(&self->pulse1);
    pulse_init(&self->pulse2);
    triangle_init(&self->triangle);
    noise_init(&self->noise);
    dmc_init(&self->dmc);

    self->sample_rate = frequency;
    self->audio_buffer_index = 0;
    self->frame_counter = 0;
    self->audio_buffer_size = AUDIO_BUFFER_SIZE;

    filter_init_high_pass(&self->filter1, self->sample_rate, 90.0f);
    filter_init_high_pass(&self->filter2, self->sample_rate, 440.0f);
    filter_init_low_pass(&self->filter3, self->sample_rate, 14000.0f);
}

inline float clamp(float d, float min, float max) {
  const float t = d < min ? min : d;
  return t > max ? max : t;
}

u8 apu_get_sample(APU* self) {
    // https://www.nesdev.org/wiki/APU_Mixer
    u8 p1 = pulse_output(&self->pulse1);
    u8 p2 = pulse_output(&self->pulse2);
    u8 t = triangle_output(&self->triangle);
    u8 n = noise_output(&self->noise);
    u8 d = dmc_output(&self->dmc);

    float pulse_out = PULSE_MIXER_LOOKUP[p1 + p2];
    float triangle_out = TRIANGLE_MIXER_LOOKUP[3 * t + 2 * n + d];
    float sample = pulse_out + triangle_out;

    sample = filter_output(&self->filter1, sample);
    sample = filter_output(&self->filter2, sample);
    sample = filter_output(&self->filter3, sample);

    // normalize to [0, 1] (constants found by running the game and measuring the output)
    sample = clamp((sample + 0.325022f) * 1.5792998016399449f, 0.0f, 1.0f);

    return (u8)(255.0f * sample);
}

void apu_write(APU* self, u16 addr, u8 value) {
    switch (addr) {
        // APU_Pulse 1
        case 0x4000: {
            pulse_write_control(&self->pulse1, value);
            break;
        }
        case 0x4001: {
            pulse_write_sweep(&self->pulse1, value);
            break;
        }
        case 0x4002: {
            pulse_write_reload_low(&self->pulse1, value);
            break;
        }
        case 0x4003: {
            pulse_write_reload_high(&self->pulse1, value);
            break;
        }
        // APU_Pulse 2
        case 0x4004: {
            pulse_write_control(&self->pulse2, value);
            break;
        }
        case 0x4005: {
            pulse_write_sweep(&self->pulse2, value);
            break;
        }
        case 0x4006: {
            pulse_write_reload_low(&self->pulse2, value);
            break;
        }
        case 0x4007: {
            pulse_write_reload_high(&self->pulse2, value);
            break;
        }
        // APU_Triangle
        case 0x4008: {
            triangle_write_setup(&self->triangle, value);
            break;
        }
        case 0x4009: {
            break;
        }
        case 0x400A: {
            triangle_write_timer_low(&self->triangle, value);
            break;
        }
        case 0x400B: {
            triangle_write_timer_high(&self->triangle, value);
            break;
        }
        // APU_Noise
        case 0x400C: {
            noise_write_control(&self->noise, value);
            break;
        }
        case 0x400E: {
            noise_write_period(&self->noise, value);
            break;
        }
        case 0x400F: {
            noise_write_length(&self->noise, value);
            break;
        }
        case 0x400D: {
            break;
        }
        // DMC
        case 0x4010: {
            dmc_write_control(&self->dmc, value);
            break;
        }
        case 0x4011: {
            dmc_write_output(&self->dmc, value);
            break;
        }
        case 0x4012: {
            dmc_write_sample_addr(&self->dmc, value);
            break;
        }
        case 0x4013: {
            dmc_write_sample_len(&self->dmc, value);
            break;
        }
        // Control
        case 0x4015: {
            pulse_set_enabled(&self->pulse1, (value & 1) != 0);
            pulse_set_enabled(&self->pulse2, (value & 2) != 0);
            triangle_set_enabled(&self->triangle, (value & 4) != 0);
            noise_set_enabled(&self->noise, (value & 8) != 0);
            dmc_set_enabled(&self->dmc, (value & 16) != 0);
            dmc_clear_interrupt_flag(&self->dmc);
            break;
        }
        // Frame counter
        case 0x4017: {
            break;
        }
    }
}

void apu_step_timer(APU* self) {
    pulse_step_timer(&self->pulse1);
    pulse_step_timer(&self->pulse2);
    triangle_step_timer(&self->triangle);
    noise_step_timer(&self->noise);
    dmc_step_timer(&self->dmc);
}

void apu_step_envelope(APU* self) {
    pulse_step_envelope(&self->pulse1);
    pulse_step_envelope(&self->pulse2);
    triangle_step_linear_counter(&self->triangle);
}

void apu_step_length_counter(APU* self) {
    pulse_step_length_counter(&self->pulse1);
    pulse_step_length_counter(&self->pulse2);
    triangle_step_length_counter(&self->triangle);
    noise_step_length_counter(&self->noise);
}

void apu_step_sweep(APU* self) {
    pulse_step_sweep(&self->pulse1);
    pulse_step_sweep(&self->pulse2);
}

const usize CYCLES_PER_FRAME = CPU_FREQUENCY / FRAME_RATE;

void apu_step_quarter_frame(APU* self) {
    static usize quarter_frame_counter = 0;

    /* sequencer steps */
    self->frame_counter = (self->frame_counter + 1) % 5;  // TODO: confirm 4‑step never used
    if (self->frame_counter & 1) {
        apu_step_envelope(self);
    } else {
        apu_step_length_counter(self);
        apu_step_envelope(self);
        apu_step_sweep(self);
    }

    usize cycles = CYCLES_PER_FRAME / 4;
    usize spf_quarter = self->sample_rate / (4 * FRAME_RATE);
    usize spf_frame = self->sample_rate / FRAME_RATE;
    usize to_write = (quarter_frame_counter == 3)
                      ? (spf_frame - 3 * spf_quarter)
                      : spf_quarter;

    usize acc = 0;
    for (usize i = 0; i < cycles; ++i) {
        acc += to_write;
        if (acc >= cycles) {
            acc -= cycles;
            if (self->audio_buffer_index < AUDIO_BUFFER_SIZE) {
                self->audio_buffer[self->audio_buffer_index++] = apu_get_sample(self);
            }
        }

        apu_step_timer(self);
    }

    quarter_frame_counter = (quarter_frame_counter + 1) & 3;
}

void apu_step_frame(APU* self) {
    // step the frame sequencer 4 times per frame
    // https://www.nesdev.org/wiki/APU_Frame_Counter

    for (usize i = 0; i < 4; i++) {
        apu_step_quarter_frame(self);
    }
}

void apu_fill_buffer(APU* self, u8* cb_buffer, usize size) {
    while (self->audio_buffer_index < size) {
        apu_step_quarter_frame(self);
    }

    usize len = size > self->audio_buffer_index ? self->audio_buffer_index : size;

    memcpy(cb_buffer, self->audio_buffer, len);
    self->audio_buffer_index -= len;
    memcpy(self->audio_buffer, self->audio_buffer + len, self->audio_buffer_index);
}