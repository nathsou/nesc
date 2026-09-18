# nesc

nesc (pronounced [nessy](https://github.com/nathsou/nessy)) is a NES emulator in C ported from my work on [this Super Mario Bros static recompilation project](https://github.com/nathsou/smb).

## Controls

- D-Pad: WASD
- B: K
- A: L
- start: Enter
- select: Space

## Building

### Linux & MacOS

In the root folder:

1. Run `git submodule init` and `git submodule update` to get `raylib`
2. Follow the raylib [build instructions](https://github.com/raylib-extras/raylib-quickstart)
3. Run `make build`

## Recording and benchmarking input

Start the emulator with `--record` to save one controller byte for each emulated frame. The trace includes the neutral frames used to prime audio and frames used to catch up after a delayed presentation. Close the emulator window normally to finish the trace.

```sh
./nesc --record /tmp/smb-inputs.nesct "path/to/Super Mario Bros.nes"
```

Build the headless benchmark and replay the same trace from a fresh power-on state for each run:

```sh
make headless-bench
./nes-bench "path/to/Super Mario Bros.nes" /tmp/smb-inputs.nesct 5
```

The benchmark performs one warm-up replay, then reports per-run and median wall-clock and process CPU frames per second. Use CPU time when comparing optimizations because it excludes time the process spends waiting to be scheduled. The final framebuffer hash helps check that builds produce the same image. Use `./nes-bench ROM TRACE --verify` for an untimed replay that prints a hash for every frame, and compare its output between builds. ROM loading, display, and audio playback are outside the timed section; CPU, PPU, and APU emulation are included.

To estimate how replay time is divided among CPU, PPU, and APU work, build the sampling-instrumented benchmark. It uses a 1 ms process-CPU sampling timer, so component percentages are estimates; the regular emulator and benchmark builds do not include this instrumentation.

```sh
make component-bench
./nes-component-bench "path/to/Super Mario Bros.nes" /tmp/smb-inputs.nesct 5
```

See [PPU benchmark results](docs/ppu-performance.md) for the optimization measurements and their limitations.

## Frame pacing

Presentation uses vsync without an additional software FPS limiter. Emulation follows elapsed time, matching 60 Hz on common 60/120/240 Hz displays and using native NTSC timing on other refresh rates. Small audio-clock differences are corrected with gradual playback-rate adjustments, capped at ±0.5% around the selected video rate. Audio queue levels never trigger extra game frames. Catch-up after a real stall is limited to four frames.

Use `--pacing-stats` to log draw rate, emulated frame rate, repeated draws, catch-up draws, the longest loop interval, audio queue depth, playback rate, and underruns once a second:

```sh
./nesc --pacing-stats "path/to/Super Mario Bros.nes"
```

Draw rate measures application submissions, not physical display refresh. Repeated draws are expected on high-refresh displays; for example, 120 draws/s with 60 emulated frames/s normally repeats each image once. A refresh rate that is not a multiple of the game rate cannot display every game frame for an identical duration. Long OS or graphics stalls can still cause catch-up frames and audio underruns.

## Mappers
- [x] NROM
- [x] MMC1
- [x] UxROM
- [x] MMC3
