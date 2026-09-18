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

## Frame pacing

Presentation uses vsync without an additional software FPS limiter. Emulation follows elapsed time, matching 60 Hz on common 60/120/240 Hz displays and using native NTSC timing on other refresh rates. Small audio-clock differences are corrected with gradual playback-rate adjustments, capped at ±0.5% around the selected video rate. Audio queue levels never trigger extra game frames. Catch-up after a real stall is limited to four frames.

Use `--pacing-stats` to log draw rate, emulated frame rate, repeated draws, catch-up draws, the longest loop interval, audio queue depth, playback rate, and underruns once a second:

```sh
./nesc --pacing-stats "/Users/nathan/Documents/keep/roms/nes/Super Mario Bros.nes"
```

Draw rate measures application submissions, not physical display refresh. Repeated draws are expected on high-refresh displays; for example, 120 draws/s with 60 emulated frames/s normally repeats each image once. A refresh rate that is not a multiple of the game rate cannot display every game frame for an identical duration. Long OS or graphics stalls can still cause catch-up frames and audio underruns.

## Mappers
- [x] NROM
- [x] MMC1
- [x] UxROM
- [x] MMC3
