# PGO experiment (2026-11-03): not worth shipping

Result of measuring [ADR-15737](adr/15737-raku-level-call-path-footprint.md) D3 (profile-guided
optimization) on the AttrX::Mooish loop of [#15688](https://github.com/tokuhirom/mutsu/issues/15688).
**Do not repeat this experiment** unless the interpreter's code layout changes fundamentally
(e.g. a different dispatch design) or the toolchain gains a materially better PGO/BOLT story.

## Verdict

PGO is worth at most ~15% on the Mooish loop even when that loop is in the training set, and
nothing measurable when it is held out. The loop is ~4x rakudo in instructions and a further ~3x
in instruction speed; PGO does not close that second factor. D3's own condition ("ships only on a
measured gain") is not met, so no release-pipeline change was made.

## Setup

- rustc 1.99.0, `rustup component add llvm-tools` (the distro `llvm-profdata` is LLVM 18 and
  cannot read rustc's LLVM 23 profiles; use the one under the toolchain's `lib/rustlib/*/bin`).
- Instrumented build: `RUSTFLAGS="-Cprofile-generate=<dir>" CARGO_PROFILE_RELEASE_INCREMENTAL=false
  cargo build --release --bin mutsu --target-dir target/pgo-gen` (~10 min on 4 cores).
- Training run (~42 s): `benchmarks/bench-*.raku` except threads/json-fast/yaml, `fib`,
  `method-call`, `poly-call`, `hash-access`, `string-concat`, `word-count`,
  `call-path/plain-call`, plus every 12th `t/*.t` file under `lang collections control exceptions
  grammar types routines regex oo`, each run once with the instrumented binary.
- `llvm-profdata merge -o x.profdata <dir>`, then `RUSTFLAGS="-Cprofile-use=x.profdata"` with the
  same non-incremental release build (~10 min). Baseline: the same build without the flag. No
  hardening or LTO flags were changed.
- Measurement: `benchmarks/call-path/mooish-lazy.raku` (real AttrX::Mooish 1.0.10),
  `CALLPATH_N=600`, user time, 7 runs per build interleaved, module cache warmed first.

## Results (user seconds, sorted)

| Build | Mooish in training | Runs | Median |
| --- | --- | --- | ---: |
| release | - | 1.12 1.18 1.23 1.27 1.41 1.43 1.46 | 1.27 |
| PGO | no (held out) | 1.23 1.28 1.33 1.38 1.44 1.50 1.58 | 1.38 (no gain) |
| release | - | 1.10 1.12 1.16 1.18 1.22 1.31 1.32 | 1.18 |
| PGO | yes (150 iterations x3) | 0.96 0.98 0.99 1.00 1.04 1.06 1.06 | 1.00 (-15%) |

Small `benchmarks/bench-*` programs moved 0-40%, but at 40-60 ms per run that is noise.

## Caveats

- Wall clock on this box drifts by more than 10% between sessions; the pairs above were taken
  minutes apart. `perf` is not installed in the remote container, so cycle and I-cache counters
  were not read.
- A shipped PGO build would also need a training run on each of the four release targets' own
  architecture (ADR-15737 D3 constraints) and a `.github/` change that waits for CODEOWNERS
  review; none of that was built.
