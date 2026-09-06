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

## Current state (2026-09-06, third pass — the perf blocker)

`todo/tickets/eval-assign-loses-a-later-block-declarations-type.md`, which the
second sweep's two new roast rows were bisected to, was closed the same day by
`news/2026-09/typed-declaration-hoist-missing-in-most-block-forms.md`, so the
correctness residue is back to the single known timeout.

That timeout was then attacked directly, with callgrind rather than A/B
experiments, and the attribution turned out to be neither of the two candidates
the earlier sections name. **The whole per-assertion cost was dominated by
multi-candidate resolution**: `resolve_function_with_types` was 29% of a
300-assertion program, re-run on every single `ok`/`is`/`is-deeply` call,
because three separate things kept a `Test` assertion out of the sound
multi-resolution cache (the parser's callsite-line marker, the `:D`/`:U`
smileys, and `VarRef` arguments). Fixing all three —
`news/2026-09/multi-resolve-cache-keys-carry-definedness-and-declared-type.md` —
took `roast/S03-buf/write-int.t` under the real module from **48.0 s to
29.5 s**, with 184 079 full resolves down to 22 805 (all of them now
per-*subtest*, not per-assertion). A second pass on the scoped-overlay return
merge
(`news/2026-09/scoped-overlay-return-merge-stops-paying-for-a-flattened-env.md`)
took it further, to **27.4 s** (median of eight; 26.5-28.1 s with one 30.4 s
outlier). In deterministic terms — callgrind, per assertion, baseline
subtracted — the two together are **1.06 M -> 535 k instructions**.

Calibration measured at the same time, same idle box, same output sink, all
three running the file's identical 93 370 assertions / 2 530 subtests:

| provider | wall | per assertion |
| --- | --- | --- |
| rakudo | 3.49 s | 37 us |
| mutsu, native `Test` | 5.04 s | 54 us |
| mutsu, vendored `Test` | 27.4 s | 293 us |

The interpreter is at rakudo's rough parity on this file; the remaining ~8x is
the cost of *running `Test.rakumod` as Raku code*.

**Completion criterion 2 is therefore still NOT met.** 27.4 s against a 30 s
per-file budget is a ~9% margin on an idle box, and single samples reached
30.4 s: the file can still time out on a slower CI runner or under `prove -j4`.
Do not read the sweep going green here as the class being closed -- re-measure
on the reference machine and under contention. The next measured target is filed as
`todo/perf/listop-call-bypasses-every-compiled-call-cache.md`: a listop call
(`ok 1, "x"` compiles to `ExecCallPairs`, not `CallFunc`) reaches none of the
three name-keyed compiled-call caches and takes the carrier path — a whole-frame
env snapshot plus a writeback diff — on **every assertion of every roast file**.
That, and the pre-existing
`todo/perf/method-dispatch-flattens-the-env-on-every-call.md` (measured at 17%
of the `ok` loop by an unsound flatten-removal experiment), are what remain.

## Earlier residue (2026-09-06, second sweep)

Both sweeps were re-run at the end of 2026-09-06, after that day's fixes. The
`t/` regression class is **empty** for the first time; the roast side has the
known timeout plus two rows a same-day refactor introduced.

### `t/` sweep (debug, all 3711 files)

```
pass under both:                   3688
regressed under the real Test:     0
passes only under the real Test:   0
fail under both (pre-existing):    23
```

All four rows of the morning's sweep are closed: `t/list-str-calls-element-str.t`
by the Seq fix, `t/match-vars-are-routine-scoped.t` and
`t/undeclared-routine-suggests-unit-own-subs.t` by
`news/2026-09/routine-match-scope-survives-an-eval-in-the-program.md`, and
`t/closure-capture-cell-dichotomy.t` by PR #7367 (the ADR-0055 unvouched-capture
fix), exactly as the analysis below predicted — it was never a `Test` blocker.

### Roast sweep (release, whitelist)

```
pass under both:                   1429
regressed under the real Test:     3
passes only under the real Test:   0
fail under both (pre-existing):    4
```

- `roast/S03-buf/write-int.t` — `exit 124`, the known timeout. See the
  performance section below.
- `roast/S04-declarations/my-6e.t` #61 and `roast/6.c/S04-declarations/my-6c.t`
  #62, both "also a type error" — **new, and not a `Test` problem**. Bisected to
  `e49b300` / PR #7364 (ADR-0042 slice 3, retiring the name-keyed
  type-constraint side table): an EVAL'd `$x = "abc"` stops seeing the type of a
  `my Int $x` declared later in the same block. Filed as
  `todo/tickets/eval-assign-loses-a-later-block-declarations-type.md`. CI cannot
  see it because the dual-provider sweep is not part of CI.

## Earlier residue (2026-09-06, first sweep — before the multi-cache fix)

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
regressed under the real Test:     4 -> 1 after the fixes below
passes only under the real Test:   0
fail under both (pre-existing):    23
```

All four were verified to reproduce identically on `origin/main`, so none was
introduced by the perf work of 2026-09-05. `t/list-str-calls-element-str.t` is
the one the Seq fix closed. **Two of the remaining three were closed on
2026-09-06**, and both turned out to be general interpreter bugs that had
nothing to do with `Test` — the module only latched the condition that exposed
them (`news/2026-09/routine-match-scope-survives-an-eval-in-the-program.md`):

- `t/match-vars-are-routine-scoped.t` #8 — **fixed**. The zero-argument compiled
  fast call installed its scoped env overlay only while
  `reflective_name_access_possible()` was false. That flag is process-global and
  monotonic, so ONE `EVAL` anywhere in a program removed the boundary from every
  zero-local routine in it, and such a routine's `reset_capture_env_vars` — which
  REMOVES inherited `$<name>` keys — then deleted the CALLER's named captures.
  Every file that loads the real `Test` latches the flag (`throws-like` EVALs a
  string), which is why it was real-provider-only. Reproduced with a bare
  `EVAL '1'` and no Test module at all; pinned by
  `t/match-vars-scoped-under-eval.t`.
- `t/undeclared-routine-suggests-unit-own-subs.t` #1,#2 — **fixed**. Not a
  provider difference in the check: the EVAL-time undeclared-routine check drew
  its suggestion candidates from the registry alone, so `EVAL 'sub greeting {};
  greetng()'` lost the suggestion under BOTH providers. The native `throws-like`
  simply does not go through EVAL, so only the real module's EVALing
  implementation exposed it. The EVAL path now passes the EVAL'd unit's own
  routine declarations, as the mainline CHECK-time walker already did.
- `t/closure-capture-cell-dichotomy.t` #7 — **still open, and now identified**:
  it is the *known-open* env-resident half of ADR-0055 section 1.2(b), which the
  file's own comment already records as open and blocked on
  `todo/deep/unvouched-capture-cells-leak-state-across-cro-client-requests.md`.
  The reflective flag is again the bridge: with it latched, `SetLocal`'s
  `skip_env_write` is disabled, so the *slot*-resident variant the test pins
  becomes the env-resident variant that was already failing. Repro without any
  Test module:

  ```raku
  my $z = EVAL "1";                 # latches the flag
  sub noop($v) { 1 }
  my $b = "OUTER";
  noop($b);                         # the vouch refusal
  my $f = { $b };
  sub collide-slot() { my $b = "CALLER"; $f.() }
  say collide-slot();               # 'CALLER', rakudo says 'OUTER'
  ```

  So this row is NOT a separate real-`Test` blocker: closing ADR-0055 §1.2(b)
  closes it. Do not chase it from the `Test` side.

### Performance: one file left, and the attribution has moved again

`S03-buf/read-write-bits.t` is **no longer** a timeout — it now completes in
~16 s here (so ~8 s on the reference machine). Only `write-int.t` remains, at
~45 s here after the 2026-09-06 dispatch fix, 48 s before it (so ~23 s on the
reference machine, against a 30 s budget), versus 4.4 s under the native
provider. It runs ~93 000 assertions, which is why it is the
last one standing: at ~0.31 ms per assertion that is ~29 s of pure assertion
overhead.

The 2026-09-05 work (`news/2026-09/nqp-and-name-dispatch-fast-paths.md`) halved
the per-assertion cost — 2000 `ok 1, "x"` assertions went 1.310 s -> 0.650 s
release, 6.22 G -> ~2.5 G retired instructions — by removing the registry walks
`nqp::` ops and lone-`multi` resolution were paying. **Do not restart from the
`&`-sigil framing in `todo/perf/interpreter-call-path-in-hot-loops.md`; that
section is stale, and so is the `nqp::`/`has_multi_candidates` diagnosis, which
is now fixed.**

**Correction (2026-09-06): target 1 below was mis-attributed, and it has been
partly overtaken.** `proclaim`'s 280 110 by-name resolutions were NOT the
defaulted-parameter light-call gate — `proclaim` never reached that gate, because
it is imported and so is absent from the caller's `compiled_fns` entirely. It
was excluded from the `otf_call_cache` instead (that cache skipped every
*plan-compiled* def, which is every module sub), so each call re-ran three full
registry walks. Fixed:
`news/2026-09/imported-module-sub-reaches-the-cached-dispatch.md` — a simple
imported sub went 14.7 us -> 0.9 us per call, at parity with a local one, and
`proclaim` now resolves 3 times in total rather than 3 times per assertion.
`write-int.t` went 48.2 s -> 45.4 s here, so **the resolutions were only ~6% of
that file**: the per-assertion cost is dominated by something else. Two
measurements say where to look next:

- **The cost is linear in env size, and the site is the METHOD path.** Adding N
  unused `our` variables to the mainline of a 2000-assertion file adds ~0.98 ns
  per entry per assertion: 0 pads 0.543 s, 300 pads 1.213 s, 600 pads 1.841 s,
  900 pads 2.363 s (release, this machine). It is
  `exec_call_method_mut_op_impl`'s unconditional `flatten_scoped_env()`, which
  rebuilds the whole lexical env for any method dispatch past the accessor fast
  path — an assertion makes two (`$output.say`, `$desc.Str`), both on native
  methods that never read a lexical by name. Commenting it out (unsound, purely
  to size the prize) takes the 900-pad file from 2.363 s to **0.958 s** and
  `write-int.t` from 45.4 s to 42.2 s. Two suspects on the SUB path were measured
  and ruled out: the `Sub` value's `clone_env` for `callframe().code` (no
  measurable change when removed) and `push_caller_env` (an `Arc` clone, not a
  flatten). Filed as
  `todo/perf/method-dispatch-flattens-the-env-on-every-call.md`.
- **`ok`/`is` are multis, so they still resolve once per call** (2001 resolves
  for 2000 `ok`s). The `otf_call_cache` deliberately excludes multi names; the
  sound multi-resolution cache misses because the arguments arrive as
  varref-captured containers, which `multi_arg_type_keys` declines to key on.

The remaining targets, unchanged in substance:

1. **A defaulted parameter disqualifies the callee from the cached light-call
   path.** (Real, but see the correction above: it is not what `proclaim` was
   paying, so do not expect the `Test` numbers to move much.) `is_positional_light_call_eligible` (`vm/vm_call_eligibility.rs`)
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
