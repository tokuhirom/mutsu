# The gc-stress and jit-stress jobs stop paying for a serial TAP run

`gc-stress` and `jit-stress` had been reporting **cancelled** — not failed — with every
test step green, because they were habitually spending ~90% of `timeout-minutes: 30` and a
single slow `apt` step was enough to push them over. The timeout had already been raised
20 → 25 → 30 for the same reason. [#7956](https://github.com/tokuhirom/mutsu/issues/7956)
asked for the structural fix rather than a fourth raise.

## Where the time actually went

Measured off CI run 34562755709 (green, attempt 2), the same commit across all three native
jobs:

| step | `test` | `gc-stress` | `jit-stress` |
|---|---|---|---|
| `prove t/` — the same 3997 files | **1m28s** (`-j4`, release) | **15m53s** (serial, debug) | **18m42s** (serial, debug) |
| `cargo build --release` | 4m25s | 4m19s | 4m32s |
| `cargo test` | 2m37s | 2m39s | — |
| `Install prove + mold` | 13s | 22s | 17s |
| **job total** | **18m13s** | **27m06s** | **28m30s** |

The `test` job runs that suite in a minute and a half. The two stress jobs were running it
**serially** — a 10.8x and 12.7x gap, of which the debug binary explains only ~3x. Nothing
in `ci.yml`, in ADR-0075, or in `CLAUDE.md` gave a reason for the serial run; ADR-0075
described it as applying "more wall-clock pressure" than the parallel one, which is
backwards, since a serial run hands each file the whole runner.

## What changed

**Both stress TAP steps run at `-j4`**, matching their own roast steps, the `test` job and
the ubuntu-latest core count. Nothing is given up: `MUTSU_GC`, `MUTSU_JIT` and their knobs
are per-process runtime settings read by each `mutsu` that prove spawns, so every file is
still exercised under exactly the configuration its job exists to test. `MUTSU_T_TIMEOUT`
goes 60 → 90, because the per-file budget now covers CPU contention from three sibling test
processes on top of the debug binary, where before it covered the debug binary alone —
`docs/flaky-test-policy.md` §3's "widen the budget" in preference to quarantining a
slow-but-correct test.

**No job runs `apt-get update` any more.** It was the most variable step in the workflow
(22s on a good day, 2m54s on the bad one that triggered the issue, and with a history of
hanging outright until the job timeout cancels it — 2026-08-19, PRs #6684 #6691). Of the
three packages it was installing, two were already on the runner image: `perl` arrives as a
`dpkg-dev` dependency and brings `prove` with it, and `libssl-dev` is in toolset-2404's
`apt.common_packages`. Only `mold` was genuinely missing, and it is a 6 MB tarball from its
own GitHub release, installed by `rui314/setup-mold` (mold's author; pinned by SHA like
every other action here) with `make-default: false`, since every cargo invocation already
selects mold explicitly through `RUSTFLAGS`. The two preinstalled packages keep a guarded
apt fallback — `command -v prove` / `dpkg -s libssl-dev` — so a future image that drops one
degrades to the old cost instead of failing every TAP step at once.

Expected: `gc-stress` and `jit-stress` at roughly 16 minutes against the unchanged 30 minute
budget, instead of 27 and 28.5.

## The `-j4` switch was verified before pushing, in both configurations

Both stress configurations were reproduced in full on a 4-core container, `t/` at `-j4` on
`target/debug/mutsu`, with `MUTSU_T_TIMEOUT=90`:

| configuration | result | wall clock | CPU |
|---|---|---|---|
| `MUTSU_GC=on MUTSU_GC_EVERY_CANDIDATE=1024 MUTSU_GC_VERIFY=1` | **PASS**, 3997/3997, 42534 tests | 278s | 1000s |
| `MUTSU_JIT=on MUTSU_JIT_THRESHOLD=2` | **PASS**, 3997/3997, 42534 tests | 314s | 1142s |

No file timed out in either run, which was the one risk in the change. The CPU totals also
confirm what the CI numbers implied: 1000s and 1142s of CPU against CI's serial wall clocks
of 953s and 1122s means the serial run *was* the CPU total, and `-j4` divides it by ~3.6.

## What was measured and deliberately *not* done

**Replacing `prove` with a Python runner** was considered and rejected on measurement. With
a stub TAP binary standing in for the interpreter, so that only harness cost is being timed,
all 3997 `t/` files at `-j4` cost **10.9s** through the full current chain (`prove` +
`run-t-test.sh` + `flaky-retry.sh` + `timeout`) and **10.0s** with prove invoking the binary
directly. The two bash wrappers are therefore worth 0.9s, and the whole harness ~11s of an
88s step. That does not pay for reimplementing `--state=save` / `--state=failed`, the
flaky-quarantine retry and the per-file timeout — and `scripts/*.py` sits on
`scripts/ci-docs-only.sh`'s documentation allowlist, so a Python runner would have to come
out of it, since a test runner is emphatically a build input.

**Moving the `mutsu-lsp` step (1m52s) off the `test` job** was also rejected, for a reason
recorded in `crates/mutsu-lsp/Cargo.toml`: the crate deliberately depends on `mutsu` with
its *default* features precisely so CI never compiles the interpreter a third time. The
step is cheap only because it reuses the artifact the preceding clippy and unit-test steps
already built. `lint-configs` compiles three other configurations and none of them is that
one, so moving it there would have bought 1m52s off a job that is no longer the constraint
by paying for a whole extra interpreter build.

**Sharing one release binary across the three jobs** (they compile it three times, ~13
minutes of runner time) and **splitting each stress job into its TAP and roast halves**
(issue option 3) are both still worth doing, and are left to a separate change: the required
status checks are job *names*, so a split needs an aggregator job that keeps the `gc-stress`
name or branch protection silently stops matching.
