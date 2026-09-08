# ADR-0075: `make test` runs the TAP (`t/`) suite on the release binary

- **Status**: Accepted
- **Date**: 2026-09-08
- **Supersedes**: [ADR-0014](0014-make-test-runs-tap-on-debug-binary.md)
- **Related**: `Makefile` (the `test` target), `.github/workflows/ci.yml` (the `test`,
  `gc-stress` and `jit-stress` jobs), `scripts/run-t-test.sh`, CLAUDE.md
  "Checking `make test` / `make roast` results".

## 1. Context

ADR-0014 (2026-07-26) moved the `t/` suite from the release binary to the debug one. Its
whole case rested on one measurement: a release build of the `mutsu` crate cost **19 m 17 s**
against a debug build's 31.8 s, to buy a t/ runtime saving of about 4 minutes
(6 m 43 s debug vs 2 m 52 s release, 2469 files). Trading a 19-minute build for a
4-minute runtime win is obviously bad, and the ADR chose debug.

Both halves of that trade have since moved, in opposite directions.

### The release build is no longer 19 minutes

That figure was accurate when it was taken. It predates the build-side work done since —
**mold** as the linker across every cargo invocation (landed 2026-09-06, and CI sets
`RUSTFLAGS: -C link-arg=-fuse-ld=mold` for the whole job), and CI's
`CARGO_PROFILE_RELEASE_DEBUG=false`, which the ADR notes cuts the binary from ~250 MB to
~27 MB and saves ~70 s of codegen/linking. Linking a binary this size dominated the
rebuild, and that is exactly the part that got fixed.

Measured 2026-09-07 on a 4-core box, warm dependency cache, full recompile of the `mutsu`
crate, with the repo's own `[profile.release] debug = true` still on and **no mold** (this
box has none installed), so CI's build is faster still:

| | this box, 2026-09-07 | ADR-0014, 2026-07-26 |
| --- | --- | --- |
| `cargo build --release` | **4 m 40 s** | 19 m 17 s |

The measurement did not become wrong; the thing it measured got four times faster. The
decision that rested on it is what does not survive.

### The runtime gap is not 4 minutes

`t/` has grown from 2469 files to 3825, and the vendored upstream `Test` module
(`todo/deep/vendor-real-test-module-flip.md`) made the *per-process*
cost of `use Test` the suite's dominant term — and that cost is **5.7x worse in debug**
(87 ms vs 15 ms per file, measured in isolation) because it is Raku code being compiled
and run, not a Rust fast path. The whole suite, cold precomp cache, `-j4`, through
`scripts/run-t-test.sh`:

| binary | `-j4` (CI's shape) | serial (`make test`'s shape) |
| --- | --- | --- |
| debug | 221 s | 1097 s |
| **release** | **67 s** | **181 s** |

3.3x parallel, **6.1x serial** — and it will keep widening as `t/` grows, because the term
that dominates it is per-file rather than per-assertion. Locally that is 15 minutes saved
against a 4 m 40 s build.

### In CI the release build is not an extra cost at all

The `test` job already built release for `make roast`, *after* running the TAP suite on a
separate debug build. So it compiled the crate twice, and the second build was for a suite
step that had already finished. Moving the release build ahead of the TAP step and pointing
that step at it removes one full compile from the job outright.

## 2. Decision

- **`make test` builds and runs `t/` on the release binary.** The `test` target becomes
  `cargo build --release && cargo test -- --test-threads=1 && cargo test -p mutsu-lsp &&
  MUTSU_BIN=$(CARGO_TARGET_DIR)/release/mutsu MUTSU_T_TIMEOUT=60 prove -e
  'scripts/run-t-test.sh' t/`.
- **CI's `test` job does one build, not two.** "Build (debug, for TAP tests)" is deleted;
  "Build (release, for roast)" moves ahead of the TAP step, is renamed, and the TAP step's
  `MUTSU_BIN` becomes `target/release/mutsu`. `MUTSU_T_TIMEOUT` stays 45.
- **`gc-stress` and `jit-stress` keep running `t/` on the debug binary**, unchanged. This
  is deliberate — see below.
- **`make roast` is unchanged.**

## 3. `debug_assert!` coverage is why the stress jobs stay on debug

The one real thing a release TAP run gives up is the 75 `debug_assert!`s in `src/`, which a
release build compiles out. That is not a theoretical concern: those assertions are in the
opcode/VM paths, and 3825 Raku files exercise them far more broadly than the Rust unit
tests can.

Two things keep the coverage:

1. **`cargo test` still runs in debug**, in every job and in `make test`. The 959 lib unit
   tests keep their assertions.
2. **`gc-stress` and `jit-stress` still run the whole `t/` suite on `target/debug/mutsu`**,
   serially. Those jobs exist to run the suite under adverse configurations, and a
   debug build with its assertions live is exactly such a configuration. Every push
   therefore still gets a suite-wide `debug_assert!` pass — twice — just not on the
   critical path of the fast gate.

This is the substantive difference from a blanket switch, and it is why "run everything on
release" was not adopted.

## 4. Consequences

- `make test` locally: pays ~4 m 40 s of build (once; incremental after) and saves ~15 min
  of runtime on the serial `prove` run (1097 s -> 181 s). CI's `test` job loses a whole
  crate compile and runs the TAP step 3.3x faster.
- **Local and CI still agree.** ADR-0014's strongest point was that a local `make test`
  should mean what CI means. It still does: both now run `t/` on release.
- A debug-only timeout is no longer visible to the fast gate — but it is still visible to
  `gc-stress`/`jit-stress`, which run the same suite on debug and serially, i.e. under
  *more* wall-clock pressure than the old `test` job applied. The flaky-test triage
  protocol is unchanged.
- ADR-0014's other consequence — that `make test` no longer produces a release binary as a
  side effect — is reversed: it produces one again, which is what `make roast` and the
  bench scripts want anyway.

## 5. Note on ADR-0014's "Deciders"

ADR-0014 lists `tokuhirom, Claude`. The repository owner has since said that a human name
in an ADR's Deciders line should not be read as a recorded human decision — an agent wrote
it. Treat ADR-0014's rationale on its measurements alone, which is what this ADR does.
