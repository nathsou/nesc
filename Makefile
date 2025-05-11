CC = clang

CFLAGS = -g -Wall -Wextra -Wno-unused-parameter -Wno-unused-variable -Wno-unused-function -Wno-unused-value -Wconversion -Wsign-conversion -Wno-missing-braces
CFLAGS += -I./raylib-quickstart/build/external/raylib-master/src
CFLAGS += -O3 -std=c99
CFLAGS += -ferror-limit=0

# Platform-specific flags
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    RAYLIB_FLAGS = -lGL -lm -lpthread -ldl -lrt -lX11
else ifeq ($(UNAME_S),Darwin) # macOS
    RAYLIB_FLAGS = -framework OpenGL -framework Cocoa -framework IOKit -framework CoreFoundation -lm -lpthread
endif

OBJECTS = raylib-quickstart/bin/Debug/libraylib.a
SOURCES += src/main.c src/lib/*.c

.PHONY: clean

build: clean
	$(CC) $(CFLAGS) -o nesc $(SOURCES) $(RAYLIB_FLAGS) $(OBJECTS)

clean:
	rm -f nesc
