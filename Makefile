CC = clang

CFLAGS = -g -Wall -Wextra -Wno-unused-parameter -Wno-unused-variable -Wno-unused-function -Wno-unused-value -Wconversion -Wsign-conversion -Wno-missing-braces
CFLAGS += -I./raylib-quickstart/build/external/raylib-master/src
CFLAGS += -O3 -std=c11
CFLAGS += -ferror-limit=0

# Platform-specific flags
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    RAYLIB_FLAGS = -lGL -lm -lpthread -ldl -lrt -lX11
else ifeq ($(UNAME_S),Darwin) # macOS
    RAYLIB_FLAGS = -framework OpenGL -framework Cocoa -framework IOKit -framework CoreFoundation -lm -lpthread
endif

OBJECTS = raylib-quickstart/bin/Debug/libraylib.a
SOURCES += src/main.c src/frame_pacing.c src/lib/*.c
TEST_SOURCES = tests/mappers_test.c src/lib/cart.c src/lib/mmc1.c src/lib/uxrom.c src/lib/mmc3.c src/lib/ppu.c
CPU_TEST_SOURCES = tests/cpu_test.c src/lib/*.c

.PHONY: clean test rom-test

build: clean
	$(CC) $(CFLAGS) -o nesc $(SOURCES) $(RAYLIB_FLAGS) $(OBJECTS)

test:
	$(CC) $(CFLAGS) -Isrc -o frame-pacing-tests tests/frame_pacing_test.c src/frame_pacing.c -lm
	./frame-pacing-tests
	$(CC) $(CFLAGS) -Isrc/lib -o mapper-tests $(TEST_SOURCES)
	./mapper-tests
	$(CC) $(CFLAGS) -Isrc/lib -o cpu-tests $(CPU_TEST_SOURCES) -lm -lpthread
	./cpu-tests

rom-test:
	$(CC) $(CFLAGS) -Isrc/lib -o rom-test tests/rom_runner.c src/lib/*.c -lm -lpthread

clean:
	rm -f nesc frame-pacing-tests mapper-tests cpu-tests rom-test
