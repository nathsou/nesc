#include "cpu.h"

#include <stdio.h>
#include <string.h>

typedef struct {
    Mapper base;
    u8 memory[0x10000];
    u16 write_addr[8];
    u8 write_value[8];
    usize write_count;
    bool irq;
} TestMapper;

typedef struct {
    CPU cpu;
    PPU ppu;
    APU apu;
    TestMapper mapper;
} Fixture;

static int failures = 0;

#define EXPECT_EQ(actual, expected) do { \
    unsigned got_value = (unsigned)(actual); \
    unsigned expected_value = (unsigned)(expected); \
    if (got_value != expected_value) { \
        fprintf(stderr, "%s:%d: expected %u, got %u\n", \
                __FILE__, __LINE__, expected_value, got_value); \
        failures++; \
    } \
} while (0)

static u8 test_mapper_read(Mapper* mapper, u16 addr) {
    return ((TestMapper*)mapper)->memory[addr];
}

static void test_mapper_write(Mapper* mapper, u16 addr, u8 value) {
    TestMapper* test_mapper = (TestMapper*)mapper;
    test_mapper->memory[addr] = value;
    if (test_mapper->write_count < 8) {
        test_mapper->write_addr[test_mapper->write_count] = addr;
        test_mapper->write_value[test_mapper->write_count] = value;
    }
    test_mapper->write_count++;
}

static bool test_mapper_irq(Mapper* mapper) {
    return ((TestMapper*)mapper)->irq;
}

static void fixture_init(Fixture* fixture) {
    memset(fixture, 0, sizeof(*fixture));
    fixture->mapper.base.read = test_mapper_read;
    fixture->mapper.base.write = test_mapper_write;
    fixture->mapper.base.is_asserting_irq = test_mapper_irq;
    fixture->cpu.ppu = &fixture->ppu;
    fixture->cpu.apu = &fixture->apu;
    fixture->cpu.mapper = (Mapper*)&fixture->mapper;
    fixture->cpu.pc = 0x8000;
    fixture->cpu.sp = CPU_STACK_TOP;
    fixture->cpu.interrupt_disable_flag = true;
    fixture->cpu.irq_disable_for_poll = true;
}

static void set_vector(Fixture* fixture, u16 vector, u16 target) {
    fixture->mapper.memory[vector] = (u8)target;
    fixture->mapper.memory[(u16)(vector + 1)] = (u8)(target >> 8);
}

static void test_brk_return_address(void) {
    Fixture fixture;
    fixture_init(&fixture);
    fixture.mapper.memory[0x8000] = 0x00;
    set_vector(&fixture, CPU_IRQ_VECTOR, 0x9000);

    EXPECT_EQ(cpu_step(&fixture.cpu), 7);
    EXPECT_EQ(fixture.cpu.pc, 0x9000);
    EXPECT_EQ(fixture.cpu.ram[0x1fd & 0x7ff], 0x80);
    EXPECT_EQ(fixture.cpu.ram[0x1fc & 0x7ff], 0x02);
    EXPECT_EQ(fixture.cpu.ram[0x1fb & 0x7ff] & 0x10, 0x10);
}

static void test_nmi_pushes_clear_break(void) {
    Fixture fixture;
    fixture_init(&fixture);
    fixture.ppu.nmi_triggered = true;
    set_vector(&fixture, CPU_NMI_VECTOR, 0x9000);

    EXPECT_EQ(cpu_step(&fixture.cpu), 7);
    EXPECT_EQ(fixture.cpu.ram[0x1fb & 0x7ff] & 0x10, 0);
}

static void test_indexed_read_page_cycle(void) {
    Fixture fixture;
    fixture_init(&fixture);
    fixture.cpu.x = 1;
    fixture.mapper.memory[0x8000] = 0xbd;
    fixture.mapper.memory[0x8001] = 0xff;
    fixture.mapper.memory[0x8002] = 0x40;
    fixture.mapper.memory[0x4100] = 0x42;
    fixture.mapper.memory[0x8003] = 0xbd;
    fixture.mapper.memory[0x8004] = 0x00;
    fixture.mapper.memory[0x8005] = 0x41;
    fixture.mapper.memory[0x4101] = 0x24;

    EXPECT_EQ(cpu_step(&fixture.cpu), 5);
    EXPECT_EQ(fixture.cpu.a, 0x42);
    EXPECT_EQ(cpu_step(&fixture.cpu), 4);
    EXPECT_EQ(fixture.cpu.a, 0x24);
}

static void test_rmw_writes_old_then_new(void) {
    Fixture fixture;
    fixture_init(&fixture);
    fixture.mapper.memory[0x8000] = 0x0e;
    fixture.mapper.memory[0x8001] = 0x00;
    fixture.mapper.memory[0x8002] = 0x50;
    fixture.mapper.memory[0x5000] = 0x81;

    EXPECT_EQ(cpu_step(&fixture.cpu), 6);
    EXPECT_EQ(fixture.mapper.write_count, 2);
    EXPECT_EQ(fixture.mapper.write_addr[0], 0x5000);
    EXPECT_EQ(fixture.mapper.write_value[0], 0x81);
    EXPECT_EQ(fixture.mapper.write_value[1], 0x02);
}

static void test_cli_delays_irq_poll(void) {
    Fixture fixture;
    fixture_init(&fixture);
    fixture.mapper.memory[0x8000] = 0x58;
    fixture.mapper.memory[0x8001] = 0xea;
    fixture.mapper.irq = true;
    set_vector(&fixture, CPU_IRQ_VECTOR, 0x9000);

    EXPECT_EQ(cpu_step(&fixture.cpu), 2);
    EXPECT_EQ(cpu_step(&fixture.cpu), 2);
    EXPECT_EQ(fixture.cpu.pc, 0x8002);
    EXPECT_EQ(cpu_step(&fixture.cpu), 7);
    EXPECT_EQ(fixture.cpu.pc, 0x9000);
}

static void test_unofficial_opcodes(void) {
    Fixture fixture;
    fixture_init(&fixture);
    fixture.cpu.a = 0x01;
    fixture.mapper.memory[0x8000] = 0x07; // SLO $10
    fixture.mapper.memory[0x8001] = 0x10;
    fixture.cpu.ram[0x10] = 0x81;
    EXPECT_EQ(cpu_step(&fixture.cpu), 5);
    EXPECT_EQ(fixture.cpu.ram[0x10], 0x02);
    EXPECT_EQ(fixture.cpu.a, 0x03);
    EXPECT_EQ(fixture.cpu.carry_flag, true);

    fixture_init(&fixture);
    fixture.mapper.memory[0x8000] = 0x02; // KIL
    EXPECT_EQ(cpu_step(&fixture.cpu), 2);
    EXPECT_EQ(fixture.cpu.halted, true);
    EXPECT_EQ(cpu_step(&fixture.cpu), 1);
    EXPECT_EQ(fixture.cpu.pc, 0x8001);
}

static void test_every_opcode_is_dispatched(void) {
    for (unsigned opcode = 0; opcode < 256; opcode++) {
        Fixture fixture;
        fixture_init(&fixture);
        fixture.mapper.memory[0x8000] = (u8)opcode;
        fixture.mapper.memory[0x8001] = 0;
        fixture.mapper.memory[0x8002] = 0x50;
        set_vector(&fixture, CPU_IRQ_VECTOR, 0x9000);
        cpu_step(&fixture.cpu);
    }
}

int main(void) {
    test_brk_return_address();
    test_nmi_pushes_clear_break();
    test_indexed_read_page_cycle();
    test_rmw_writes_old_then_new();
    test_cli_delays_irq_poll();
    test_unofficial_opcodes();
    test_every_opcode_is_dispatched();
    if (failures != 0) {
        fprintf(stderr, "%d CPU test(s) failed\n", failures);
        return 1;
    }
    puts("CPU tests passed");
    return 0;
}
