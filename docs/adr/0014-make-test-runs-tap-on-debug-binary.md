# ADR-0014: `make test` runs the TAP (`t/`) suite on the debug binary, not release

- **Status**: Accepted
- **Date**: 2026-07-26
- **Deciders**: tokuhirom, Claude
- **Related**: `Makefile` (the `test` target), `.github/workflows/ci.yml` (the `test` job),
  `scripts/run-t-test.sh`, CLAUDE.md "Checking `make test` / `make roast` results" /
  "Delegate the full roast run to CI".

## 1. Context

Before this ADR the `make test` target built **both** a debug and a release binary:

```make
test:
	cargo build \
	 && cargo test -- --test-threads=1 \
	 && cargo build --release \
	 && MUTSU_BIN=target/release/mutsu MUTSU_T_TIMEOUT=60 prove -e scripts/run-t-test.sh t/
```

- `cargo build` + `cargo test` — the Rust `#[test]` unit tests, on the debug build.
- `cargo build --release` + `prove t/` — the ~2469-file Raku TAP suite, run against the **release**
  binary.

The release build exists in this target *only* so `prove t/` runs on an optimized binary. That build
is the single dominant cost of `make test`.

### Measurement (2026-07-26, 12-core dev box; only the `mutsu` crate recompiles — deps cached)

| Phase | Time |
|---|---|
| `cargo build` (debug, full recompile of `mutsu`) | 31.8 s |
| `cargo test -- --test-threads=1` (recompile under test cfg + run) | 5 m 02 s |
| **`cargo build --release` (full recompile of `mutsu`)** | **19 m 17 s** |
| `prove t/` (2469 files, **release** binary) | 2 m 52 s |
| `prove t/` (2469 files, **debug** binary) | 6 m 43 s |

The release compile is ~36× the debug compile (the crate is large and its optimized codegen is
extremely heavy) and accounts for roughly 70 % of the whole-target wall-clock. Yet running the TAP
suite on debug instead of release costs only ~4 more minutes of *runtime*. Trading a 19-minute build
for a 4-minute runtime delta is a clear net win: ~28 min → ~12 min.

### The release binary was never actually required here — CI already proves it

CI does **not** invoke `make test`. The `test` job in `.github/workflows/ci.yml` runs the steps
individually, and its TAP step already runs on the **debug** binary:

```yaml
- name: Build (debug, for TAP tests)
  run: cargo build
- name: TAP tests (prove t/)
  run: prove -e 'scripts/run-t-test.sh' t/
  env:
    MUTSU_BIN: target/debug/mutsu
    MUTSU_T_TIMEOUT: "30"
```

The gc-stress and jit-stress jobs likewise run `prove t/` on `target/debug/mutsu`. CI's `cargo build
--release` is used **only** for `make roast`. So the authoritative gate has always exercised the TAP
suite on debug; the local `make test` running it on release was the *outlier*, not the standard. It
was also strictly *more lenient* in one axis (release runtime + a 60 s per-file timeout vs CI's debug
runtime + a 30 s timeout), which cannot catch a debug-only timeout that CI would.

## 2. Options considered

### Option A — replace `make test` to run `t/` on the debug binary — CHOSEN

Drop `cargo build --release` from the `test` target; point `prove t/` at `target/debug/mutsu`.

- **+** Removes the 19-minute release build; `make test` drops from ~28 min to ~12 min.
- **+** **Aligns local `make test` with CI**, which already runs the TAP suite on debug. A pass/fail
  locally now means the same thing it means in CI.
- **+** No correctness loss: the TAP tests assert on program *output*, which is identical between
  debug and release; only wall-clock differs.
- **−** A per-file timeout is closer on debug (debug runtime ≈ 2.3× release). Mitigated by keeping
  `MUTSU_T_TIMEOUT=60` locally (CI uses 30 and passes), leaving generous headroom.
- **−** `make test` no longer produces a release binary as a side effect. Anyone needing one for
  roast/bench runs `make roast` or `cargo build --release` explicitly — which is already how those
  paths work.

### Option B — add a separate `make test-fast`, keep `make test` on release

Keep the release-based `make test` and add a debug-based fast variant.

- **+** Preserves the existing target verbatim.
- **−** The release-based `make test` has no unique value over CI (CI *is* the release-adjacent gate
  for roast, and runs t/ on debug), so keeping it as the default just preserves the 19-minute build
  as the thing everyone types by habit. Two targets that differ only in an axis CI does not even use
  is needless surface. The default should be the fast, CI-aligned one.

### Option C — status quo (build both)

Rejected: pays a 19-minute release build on every `make test` for a ~4-minute runtime benefit that
CI does not even rely on.

## 3. Decision

- **Adopt Option A.** `make test` runs `cargo build` + `cargo test -- --test-threads=1`, then
  `prove t/` against `target/debug/mutsu` with `MUTSU_T_TIMEOUT=60`. The `cargo build --release`
  step is removed from the `test` target.
- **`make roast` is unchanged** — it still builds and runs on the release binary
  (`MUTSU_BIN ?= target/release/mutsu`), because the roast suite is timing-sensitive (S17
  concurrency) and long enough that release runtime genuinely matters. The default `MUTSU_BIN` at the
  top of the Makefile stays release for that reason; only the `test` target overrides it to debug.
- **CI is unchanged** — it already ran the TAP suite on debug; this ADR just makes local `make test`
  match it.

## 4. Consequences

- `make test` wall-clock drops from ~28 min to ~12 min on a warm dependency cache, and local
  pass/fail semantics now match CI's TAP step exactly.
- Documentation that described `make test` as building/using the release binary for `t/` is now
  inaccurate and is corrected (CLAUDE.md "Delegate the full roast run to CI": CI runs the TAP suite on
  debug and reserves release for roast).
- If a `t/` test starts timing out under `make test` on debug, that is a real signal (the test got
  slow, or the box is loaded) — investigate per the flaky-test triage protocol rather than reaching
  back for the release binary. Confirm against a release build (`target/release/mutsu`) only to
  distinguish "genuinely too slow" from "debug-only slow".
- To get a release binary, run `make roast` or `cargo build --release` explicitly; `make test` no
  longer produces one as a side effect.
