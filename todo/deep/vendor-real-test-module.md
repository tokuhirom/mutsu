# Make the vendored upstream `Test` module the default provider

## Goal and current state

`Test` is still intercepted by the native provider in
`src/runtime/test_functions.rs`. The unmodified upstream module is already
vendored at `modules/Rakudo-Core/lib/Test.rakumod` (source hash
`f34dec45d52ad099c37f42fdbd93e277`). Setting `MUTSU_REAL_TEST=1` at process
startup loads that module instead.

The vendored module now parses, loads, and supports its full assertion surface
in normal use. Its required `nqp::` operations are implemented and pinned by
`t/nqp-process-ops.t`. Keep `Test::Util` separate: roast loads it from
`roast/packages/Test-Helpers/`, not from this provider.

The native provider remains the default until the real-module mode has no
correctness or timeout regressions across the whitelisted roast suite. Do not
add native compatibility shims to `Test.rakumod`; fix general interpreter
behaviour or the test harness instead.

## How to exercise it

`MUTSU_REAL_TEST` is read once when the interpreter starts. Use it in the
process environment, not from Raku code during a test.

```sh
MUTSU_REAL_TEST=1 target/debug/mutsu t/vendored-real-test-module.t
MUTSU_REAL_TEST=1 MUTSU_FUDGE=1 prove -e target/release/mutsu roast/S24-testing/fails-like.t
```

The normal provider remains available by omitting the variable. Do not compare
the two providers' TAP byte-for-byte: upstream `Test` intentionally emits
different, and often more faithful, diagnostics. A regression is a file that
passes with the native provider and fails with the vendored one.

## Required measurement

Run a fresh sweep before and after work that can affect real-`Test` mode. It
is not part of normal CI, so CI alone cannot detect a regression here.

```sh
cargo build
scripts/test-module-sweep.sh [jobs]                 # debug build; all t/*.t
cargo build --release
scripts/roast-test-module-sweep.sh [jobs]           # release build; whitelist
```

The scripts run each file under both providers and classify using exit status,
TAP failures, short plans, and TODO-marked failures. Read the generated
reports rather than inferring status from a raw TAP diff:

- `tmp/test-module-sweep/regressions.txt`
- `tmp/roast-real-sweep/regressions.txt`
- `tmp/roast-real-sweep/regressed-files.txt`

For an `exit 124` roast row, re-run the individual file with a larger timeout
before classifying it as a correctness bug. The vendored provider executes
assertions as Raku code and is therefore slower than the Rust-native provider.

## Current residue (2026-08-30)

The latest release sweep reported 1427 files passing under both providers,
eight initial real-provider regressions, and one pre-existing failure.
`S24-testing/fails-like.t` was the only correctness regression and has since
been fixed. Re-measure before quoting this count; the list below is the useful
handoff state.

### Performance blockers

These are timeouts under real `Test`, not known semantic divergences:

- `6.d/S32-str/sprintf-{b,d,x}.t`
- `S03-buf/{read-write-bits,write-int}.t`

The blocker is the hot function-call path, especially `&`-sigil parameters
passed through module routines. Follow
`todo/perf/interpreter-call-path-in-hot-loops.md`; do not weaken roast timeouts
or special-case `Test` to hide it.

### Native-provider-only whitelist rows

`S24-testing/2-force_todo.t` and `S24-testing/6-done_testing.t` rely on the
native provider's handling of `#?rakudo eval`. Resolve this by implementing the
needed fudge support or by removing the rows from the whitelist if they are not
valid for mutsu's supported fudge subset. This is not a vendored-`Test`
interpreter compatibility fix.

## Completion criteria

Before changing `runtime_module.rs` so `use Test` loads the vendored module by
default:

1. The local and roast sweep reports contain no real-provider correctness
   regressions.
2. The timeout class is eliminated or has a separately agreed test-budget
   solution; it must not merely be ignored by the sweep.
3. `Test::Util` still composes with the default provider.
4. Run the focused tests, then `make test` and the relevant roast checks. The
   first default-provider PR must be treated as a full-suite review.

After the switch, remove the native `Test` interception and its maintenance
burden rather than retaining two production providers indefinitely.
