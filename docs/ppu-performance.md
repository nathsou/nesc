# PPU optimization measurements

Measured on an Apple M2 with Apple clang 21.0.0, `-O3 -std=c11`, on 2026-09-18.

## Changes

1. Hoist rendering-mask decoding out of the PPU dot loop and reuse it in the tick helper. CPU writes cannot interleave dots within a `ppu_step` call. All dots, mapper ticks, delayed NMI checks, and frame transitions still execute.
2. Replace nametable address range chains with bit masks. Mirroring mode is read from the cartridge on every access, so mapper-controlled mirroring changes remain immediate.
3. Cache eight 1 KiB CHR read pointers per mapper. NROM and UxROM use fixed mappings; MMC1 and MMC3 refresh pointers when CHR mapping changes. CHR RAM pointers remain live, writes keep their mapper policy, and PPU address notifications still run before every cached read. Null pages fall back to the mapper read callback. MMC1 reset now initializes its previously uninitialized CHR bank registers before constructing pointers.

The PR also includes the previously uncommitted sprite scanline cache, input recorder, headless replay benchmark, and optional component sampler. Commit `bfb55f6` is the reproducible baseline containing those changes before the three optimizations above. `d45c88b` is main with only the frame pacing fix.

## Workload and method

The original 14,995-frame recording at `/tmp/smb-inputs.nesct` was no longer present. These measurements use **6,000 neutral-input frames**, allowing the games' built-in demos to run. They are not measurements of the user's recorded gameplay.

Each configuration runs one untimed warm-up and three timed replays from fresh NES instances. Times include CPU, PPU, and APU emulation, excluding ROM loading, video presentation, audio playback, and framebuffer verification. The headless audio buffer is not consumed. Results are medians of process CPU time, which excludes descheduling but remains sensitive to clock frequency, cache behavior, and workstation load. Separate builds contain each optimization in isolation; these percentages must not be added together.

### Super Mario Bros. (NROM)

| Configuration | Median CPU seconds | CPU time reduction vs. sprite-cache baseline |
| --- | ---: | ---: |
| Main, before sprite cache | 6.536158 | — |
| Sprite-cache baseline (`bfb55f6`) | 6.655960 | — |
| Dot-loop change only | 6.588830 | 1.0% |
| Nametable mirroring change only | 6.353787 | 4.5% |
| CHR page cache only | 6.524665 | 2.0% |
| All three changes | 6.453871 | 3.0% |

The complete PR is only **1.3% lower CPU time than main** on this demo workload. Individual runs varied: the CHR-only configuration ranged from 6.41 to 6.73 seconds. Small differences are indicative, not evidence of universal speedups. In particular, this trace does not reproduce the earlier sprite-cache improvement measured during gameplay. NROM has no CHR bank switching, so SMB alone is insufficient to assess the bank-cache opportunity.

### Kirby’s Adventure (MMC3)

| Configuration | Median CPU seconds | CPU time reduction |
| --- | ---: | ---: |
| Sprite-cache baseline | 7.461984 | — |
| All three changes | 7.299105 | 2.2% |

This exercises a banked CHR ROM and MMC3's PPU address callbacks. It still shows a modest overall gain; these measurements do not establish a large PPU throughput improvement.

## Component sample

A separate instrumented SMB replay collected 2,116 samples: CPU 13.4%, PPU 72.0%, APU 14.6%, scheduler/other below 0.1%. These are approximate shares from one run; the requested 1 ms timer is subject to OS timer resolution. The PPU remains the dominant core cost after these changes. Instrumented times are excluded from the comparison tables.

## Correctness checks

- `make test`: frame pacing, mapper, CPU, and input trace tests.
- Mapper tests under AddressSanitizer and UndefinedBehaviorSanitizer.
- Exhaustive nametable address checks across all five mirroring modes, including the `$3000` aliases.
- Cached PPU reads compared with the mapper decoder across every CHR byte, bank changes, MMC3 inversion/wrapping, resets, and live CHR RAM writes.
- MMC3 IRQ assertion through actual PPU reads and eight intervening PPU ticks, preserving A12 notifications.
- All 6,000 SMB framebuffer hashes match main in a separate untimed verification pass; final FNV-1a hash `C9E4EEDFD36EC84B`.
- All 6,000 Kirby framebuffer hashes match the sprite-cache baseline; final FNV-1a hash `98210B29D9B13472`.

These checks exercise the modified paths; matching framebuffers are not a claim of complete NES hardware accuracy.

## Reproduction

Create the same neutral trace (no ROM data is embedded):

```sh
python3 - <<'PY'
from pathlib import Path
Path('/tmp/nesc-demo.nesct').write_bytes(b'NESCINP\x01' + bytes(6000))
PY
make headless-bench
./nes-bench "path/to/Super Mario Bros.nes" /tmp/nesc-demo.nesct 3
./nes-bench "path/to/Super Mario Bros.nes" /tmp/nesc-demo.nesct --verify > /tmp/frames.txt
```

Build `bfb55f6` in a separate checkout to compare the three new changes against the sprite-cache baseline. Run binaries sequentially using the same ROM and trace. Compare `--verify` output with `cmp`; hashing is intentionally excluded from timed runs. The same benchmark harness can be built against the library sources from `d45c88b` (adding `input_trace.c/.h`), with all headers taken from the corresponding library snapshot.

For real gameplay, record a new trace using `./nesc --record trace.nesct ROM`, and save it outside `/tmp` if it needs to survive restarts. Use `make component-bench` for sampled CPU/PPU/APU shares; its instrumentation is absent from normal builds, and its timing should not be mixed with regular benchmark timings.
