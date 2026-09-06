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

## Current residue (2026-09-06)

Both sweeps were re-run on this date, on a machine roughly **2x slower** than
the one the 2026-08-29/30 numbers came from (calibration: `S04-declarations/state.t`
under the real module took 15.1 s there and 28.5 s here). Read every wall-clock
figure below with that in mind, and re-calibrate before comparing across
sessions.

### Roast sweep (release, whitelist)

```
pass under both:                   1430
regressed under the real Test:     2 -> 1 after the fix below
passes only under the real Test:   0
fail under both (pre-existing):    4
```

- `roast/integration/advent2009-day20.t` — **fixed**. `is @b, (@people.sort: {...})`
  compared an Array against a Seq whose elements define their own `.Str`, and
  the two sides were rendered by different stringifiers: the operand coercion
  reified a deferred `Seq` and returned early, skipping the element-`Str`
  resolution the Array side had already had. So `@objs.Seq eq "..."` was False
  where the identical `@objs eq "..."` was True. This was a general `eq`/`~`
  bug, not a Test one; the vendored module merely exposed it because its `is`
  IS that `eq`. Pinned by `t/list-str-calls-element-str.t` (assertions that go
  through `eq` directly, so they fail under the native provider too).
- `roast/S03-buf/write-int.t` — `exit 124`, the one remaining timeout. See
  below.
- The four fail-under-both rows (`6.c/S32-io/file-tests.t`,
  `S10-packages/precompilation.t`, `S16-filehandles/filetest.t`,
  `S32-io/IO-Socket-Async.t`) are provider-independent and were not
  investigated here; three of them are filesystem tests that a root-user
  container answers differently.

### `t/` sweep (debug, all 3692 files)

```
pass under both:                   3665
regressed under the real Test:     4 -> 3 after the fix above
passes only under the real Test:   0
fail under both (pre-existing):    23
```

All four were verified to reproduce identically on `origin/main`, so none was
introduced by the perf work of 2026-09-05. `t/list-str-calls-element-str.t` is
the one the Seq fix closed. The remaining three, each still open:

- `t/closure-capture-cell-dichotomy.t` #7 — "call-arg-sourced capture wins over
  a slot-resident same-named caller lexical".
- `t/match-vars-are-routine-scoped.t` #8 — "a sub that resets captures does not
  delete the caller `$<first>`".
- `t/undeclared-routine-suggests-unit-own-subs.t` #1,#2 — the CHECK-time
  "Did you mean 'greeting'?" suggestion is absent when the `throws-like` that
  EVALs the snippet comes from the real module rather than the native handler.
  The exception type and the die itself are right; only the suggestion list is
  empty.

### Performance: one file left, and the attribution has moved again

`S03-buf/read-write-bits.t` is **no longer** a timeout — it now completes in
~16 s here (so ~8 s on the reference machine). Only `write-int.t` remains, at
~49 s here (~25 s on the reference machine, against a 30 s budget), versus 4.4 s
under the native provider. It runs ~93 000 assertions, which is why it is the
last one standing: at ~0.31 ms per assertion that is ~29 s of pure assertion
overhead.

The 2026-09-05 work (`news/2026-09/nqp-and-name-dispatch-fast-paths.md`) halved
the per-assertion cost — 2000 `ok 1, "x"` assertions went 1.310 s -> 0.650 s
release, 6.22 G -> ~2.5 G retired instructions — by removing the registry walks
`nqp::` ops and lone-`multi` resolution were paying. **Do not restart from the
`&`-sigil framing in `todo/perf/interpreter-call-path-in-hot-loops.md`; that
section is stale, and so is the `nqp::`/`has_multi_candidates` diagnosis, which
is now fixed.** The measured next targets are:

1. **A defaulted parameter disqualifies the callee from the cached light-call
   path.** `is_positional_light_call_eligible` (`vm/vm_call_eligibility.rs`)
   requires `pd.default.is_none() && !pd.optional_marker`, so every call to a
   routine with a trailing default re-resolves by name. Measured:
   `sub f($a) {...}` called 1000x costs **1** `function-full-resolve` in total;
   `sub f($a, $b = 1, $c = 2) {...}` costs **1001**, one per call. Every
   assertion routine in `Test.rakumod` has that shape
   (`proclaim($cond, $desc, $unescaped-prefix = '')`,
   `ok(Mu $cond, $desc = '')`). This is the exact analogue of the `&`-sigil
   gate that `a7373e323` lifted.
2. **`cached_fn_package` is a stub that always returns `None`**
   (`vm/vm_call_resolve.rs`), so the non-light call path runs a full
   `resolve_function_with_types` purely to learn the callee's defining package
   — and that resolve also drives the deprecation check, so removing it needs
   the deprecation info cached alongside the package.
3. `.WHAT` always falls back to the interpreter carrier
   (`vm_call_method_compiled_mut.rs` routes the MOP pseudo-methods there):
   measured 5.3 us/call against 1.2 us for `.defined`. Small next to the two
   above (~0.5 s of `write-int.t`'s 49 s) but broad — `.WHAT` is everywhere in
   Raku code, and real `Test`'s `is` calls it twice per assertion.

Do not weaken roast timeouts or special-case `Test` to hide the remaining file.

### Native-provider-only whitelist rows

`S24-testing/2-force_todo.t` uses `#?rakudo eval` around Rakudo-only
`force_todo` calls. The roast preprocessor must skip that backend-specific
block while preserving its TAP plan. `S24-testing/6-done_testing.t` is not a
valid whitelist row: current Rakudo also rejects its `ok 0, :todo(1)` call
against `Test`'s signature, so it must remain out of the whitelist. Neither is
a vendored-`Test` interpreter compatibility fix.

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
