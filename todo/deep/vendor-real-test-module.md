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
correctness or timeout regressions across **the whitelisted roast suite, the
`t/` suite, and the bundled-library gate**. As of the 2026-09-07 sixth pass the
first two are clean and the third is not; read that section before reaching for
the one-line flip, and update the criteria at the bottom rather than the prose
here. Do not add native compatibility shims to `Test.rakumod`; fix general
interpreter behaviour or the test harness instead.

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

**The two sweeps are not the whole gate.** They cover `t/` and the roast
whitelist; they do NOT cover the bundled-library suites, which are a CI step
(`scripts/battery-testsuite.sh`, the `test` job's "Bundled-library test suites").
Run that under both providers too before believing the criteria below are met:

```sh
cargo build --release
scripts/battery-testsuite.sh                     # the vendored module, once flipped
MUTSU_REAL_TEST=0 scripts/battery-testsuite.sh   # the native-provider baseline
```

That gate is what the 2026-09-07 sixth pass caught the switch on; see below.

## Current state (2026-09-07, sixth pass -- the switch was attempted and reverted)

The flip was implemented and measured end to end.
`Interpreter::real_test_module_enabled` defaulting to `true` (with
`MUTSU_REAL_TEST=0` as the opt-out the sweeps use) is the whole switch; the
rest of the diff was three consequences of the module being faithful. It was
**reverted before landing** because a CI gate the earlier passes never ran
regresses. What the attempt established:

### `t/` and roast are genuinely clean

Full suites, this box (4 cores, `-j4`), native provider vs vendored:

| suite | native | vendored |
| --- | --- | --- |
| `prove -j4 t/` (debug, 3791 files) | 136 s wall / 435 CPU s, all pass | 280 s / 1008 CPU s, **all pass** |
| `make roast` (release, 1436 files) | 242 s / 382 CPU s | 266 s / 516 CPU s, **same 3 failures** |

The three roast rows (`6.c/S32-io/file-tests.t`, `S16-filehandles/filetest.t`,
`S32-io/IO-Socket-Async.t`) fail identically under both providers -- they are
the environmental rows every earlier sweep reports. No new timeout appeared, so
criterion 2 holds on this box under the real `make roast` job, not just under
the sweep.

Cost: roast +10% wall, `t/` +106%. The `t/` figure is the fixed per-process
module load (~71 ms with the precompilation cache warm) charged to 3791 very
small files; roast's files amortize it. The two stress CI jobs run `prove t/`
**serially**, so they absorb the ~2.3x CPU undivided and need
`timeout-minutes: 30 -> 45`.

### The bundled-library gate is where it fails

`scripts/battery-testsuite.sh` -- a step in the `test` job, so a real CI gate:

```
native provider:   283/312 test files pass   (6 DBIish MySQL rows, no server here)
vendored module:   276/312                   (those 6, plus 9 more)
```

**Six of the nine are fixed** (`news/2026-09/a-module-keeps-the-routines-it-imported.md`):
`DBIish`'s four common-testing suites, `NativeHelpers::Blob` and `NativeLibs`
were three ways a module could lose the routines *it* had imported --
`need` suppressing the exports of everything the needed module used, a scope
restore reclaiming a module's own `GLOBAL::` aliases, and `use NativeCall`
skipping its package-symbol write on a re-`use`. None of the three was about
`Test`; each reproduces on plain user modules.

**Three remain**, each a separate root cause:

- **`todo/deep/vendored-test-context-corrupts-a-sha512-digest.md`** --
  `Digest::SHA2`'s `sha512` returns a different digest for byte-identical input
  depending on process history. Bisected to a two-`subtest` repro; inputs, IV,
  round constants and multi dispatch all verified correct at the call site;
  deterministic under JIT/GC on and off. Accounts for the `Digest` row.
- **`Cro::HTTP`'s `http2-request-parser.rakutest`** -- an `ok` emitted from a
  `start` block that outlived its `test()` helper lands inside the NEXT
  assertion's `throws-like` subtest, so that subtest runs 3 tests against a
  plan of 2. The late assertion exists under both providers (the native run
  emits 61 tests, the vendored one 60); only the accounting differs, because
  the vendored `subtest` swaps module-scoped counters that every thread shares.
  The real gap is that mutsu resolves the request's `body-blob` Promise later
  than rakudo does, so the tap's `start` block is still running when `test()`
  returns.
- **`Cro::HTTP`'s `http-middleware.rakutest`** -- `throws-like { await
  Cro::HTTP::Client.get($url) }` reports "code dies" as a FAILURE: the `await`
  returns normally and `X::Cro::HTTP::Error::Client` surfaces at the top level
  afterwards, so the subtest's `CATCH` never sees it. Another await/Promise
  propagation-timing gap rather than a `Test` one.

### Three behaviour changes the switch will bring (already measured, not bugs)

Re-apply these with the flip; none of them is a reason to shim `Test.rakumod`.

1. **Failure diagnostics move to stderr.** Upstream `Test` writes
   `expected:`/`got:` through `$failure_output` (`$*ERR`); only
   `not ok N - <desc>` stays on stdout. Verified byte-identical to rakudo
   2026.07. `t/is-deeply-user-raku-diagnostic.t` asserts `:out` and must assert
   `:err`.
2. **`#?rakudo todo` starts reporting "TODO passed".** The roast fudge
   preprocessor prefixes the reason with a `__mutsu_backend_todo__:` marker that
   only mutsu's native TAP writer understands, so an assertion rakudo is known
   to fail but mutsu passes prints a bare `ok`. The vendored module knows
   nothing about that marker and must not be taught it, so
   `run_roast_preprocess.rs` should emit a plain `todo 'reason', N` (what
   rakudo's own fudge emits) and the marker handling in `call_helpers.rs` goes
   away. 71 whitelisted roast files then carry `TODO passed:` rows -- prove
   summary noise, not failures, and a true statement about mutsu.
   `src/runtime/run_dist.rs`'s three fudge-todo unit tests pin the old spelling.
3. **`t/vendored-real-test-module.t`** pins "unset means native"; it has to pin
   "unset means vendored" instead, keeping `MUTSU_REAL_TEST=0` as the native
   half.

One prerequisite of the switch is already on main:
`resolve_bundled_lib_paths` now probes `modules/` three levels up, so a
`cargo test` harness (`target/<profile>/deps/...`) sees the bundled batteries at
all -- without it every in-process `use Test` in a `#[test]` dies with "Could
not find Test".

## Current state (2026-09-07, fifth pass -- eight more callgrind-driven slices)

Same protocol as the fourth pass (callgrind only, 300 `ok 1, "x"` under
`MUTSU_REAL_TEST=1` minus the one-assertion baseline, release build), same
rule (one PR per slice, before/after in its description, nothing
special-cased for `Test`). The session opened at 335,929 Ir per assertion
(a re-measurement of the fourth pass's 333,577 on the same binary).

| slice | per assertion | PR |
| --- | --- | --- |
| start of the session | 335,929 Ir | -- |
| `nqp::` ops skip the call machinery (a compile-time `NQP_OP` symbol flag); a `Str` `.gist` answers without dispatch | 313,662 | #7472 |
| env-pure mutating methods (`push`, `AT-KEY`, `new`, ...) dispatch before the scoped-env flatten | 285,180 | #7476 |
| free-variable reads drop the `str::contains` searchers (`has_double_colon` & co. byte scans); `package_scope_lexical` short-circuits an empty store | 276,814 | #7477 |
| a named routine's `callframe().code` object is built on demand (`CodeFrame::Lazy`) | 261,339 | #7480 |
| the binder stops re-deriving the implicit `Any` check (native tags accept `Any`), a `::T`-capture latch, `is copy` skips the sigilless meta removes, `^name` placeholder key memoized | 250,138 | #7483 |
| native integer wrapping stays in `i128` machine arithmetic (`my int` RMW, typed stores, native-typed parameters) | 244,707 | #7486 |
| `CompiledFns` shares its bodies (`Arc<CompiledFunction>`), so the lazy frame holds its routine by refcount instead of cloning `params`/`param_defs` per call | 237,744 | #TBD7 |
| typed-lexical metadata probes take symbols (`var_type_constraint_sym`, the hash-key twin memoized), `resolve_constraint_alias` borrows | 234,580 | this PR |

**-30.2% in instructions per assertion this pass; -52.3% since the fourth
pass opened at 492,188.** Wall clock on this box, release build:

| | fourth pass | now |
| --- | --- | --- |
| 20,000-assertion `ok` loop, real module | 0.86 s | 0.47-0.49 s |
| same loop, native provider | -- | 0.03 s |
| `roast/S03-buf/write-int.t`, real module, idle (three runs) | 9.6-9.9 s | 7.0-7.9 s (median 7.5) |
| `write-int.t`, native provider | -- | 3.5 s |

### Completion criterion 2, re-measured under contention

`write-int.t` under the real module through `scripts/run-roast-test.sh`
with `prove -j4 --timer` alongside the same five heavy files as the fourth
pass (`S03-buf/read-write-bits.t`, `S32-str/sprintf{,-b,-e,-x}.t`):

| condition | this box | scaled to the reference machine (x2) |
| --- | --- | --- |
| idle, median of three | 7.5 s | ~15 s |
| `prove -j4`, six heavy files | 10.9 s | ~22 s |

The contended, scaled figure is now ~28% under the 30 s budget (fourth
pass: ~10%), the idle one ~50%. That is the margin the fourth pass asked
for before calling criterion 2 met on this box; the reference machine is
still not available from this session, so the x2 scaling remains an
estimate rather than a measurement.

### Sweeps (2026-09-07, after the eighth slice)

Both sweeps re-run on the binaries carrying all eight slices.

Roast sweep (release, whitelist, 4 jobs):

```
pass under both:                   1432
regressed under the real Test:     0
passes only under the real Test:   0
fail under both (pre-existing):    4
```

The four fail-under-both rows are the same environmental four as every
earlier sweep.

`t/` sweep (debug, all 3782 files, 4 jobs):

```
pass under both:                   3760
regressed under the real Test:     0
passes only under the real Test:   0
fail under both (pre-existing):    22
```

Criterion 1 (no real-provider correctness regression in either sweep)
holds; criterion 3 (`Test::Util` composes) is exercised by the roast sweep
under both providers.

### What the profile looks like now (per assertion, 234.6k)

Inclusive, after the eighth slice. One caveat the fourth pass did not
record: ~44.6k of the differential is the JIT compiling the loop's hot
ranges (`vm_jit_compile::compile_range`), a one-time cost that the
300-minus-1 protocol charges to the 299 extra assertions. The steady-state
per-assertion figure is therefore ~190k; the protocol is kept as is so the
slice-to-slice deltas stay comparable.

| row | Ir | what it is |
| --- | --- | --- |
| `bind_function_args_values` | 28.1k | the general binder for `ok`/`proclaim`: `bind_param_type_constraint` 4.4k (interns `pd.name` per parameter; `bind_param_value` interns it again), `check_and_coerce_param_type` 3.5k for `Bool(Mu)`, `bind_param_value` 3.2k, `@_`/filtered-args `Vec`s and `String` clones |
| `exec_call_method_mut_op` | 15.7k | `$output.say` and `$desc.Str`: `try_env_pure_mut_dispatch` 8.3k, `decode_arg_sources` 3.0k (a `Vec<Option<String>>` of cloned names plus a `HashMap<String, u32>` per call) |
| `exec_set_local_op` | 15.1k | four `SetLocal`s at ~3.8k: `set_env_plain_lexical` 3.5k (the env mirror + `set_shared_var_sym`), the remaining `var_type_constraint_sym` probes, `exec_set_var_dynamic_op` 3.8k |
| `Symbol::intern` | 12.7k | ~85 interns per assertion (fourth pass: ~195): the binder (`pd.name` twice per parameter), `native_lever_a_user_override`, `multi_arg_type_keys`, `set_var_type_constraint_impl`, `find_compiled_function_inner` |
| free-variable reads | 8.2k + 7.5k | `get_env_with_main_alias` -> `unit_lexical_slot` -> `lookup_in_package_chain`: ~10 reads of `$num_of_tests_run`, `$indents`, `$output`, ... at ~600 each, each re-resolving the running frame's package candidates and hashing two strings |
| `call_nqp_op` | 7.1k | five `nqp::` ops, now called directly; the remaining cost is `to_string_value` on their string operands |
| `exec_string_concat_op` | 6.8k | the TAP line: `to_string_value` (a `String` per operand) + `resolve_list_element_stringifiers` + `mixin_user_stringifier` per operand |
| `malloc` + `free` | ~31k | diffuse; every row above allocates |

The env flatten of the fourth pass is gone from the profile (`Env::flattened`
0, `drop_in_place<Env>` gone): #7476 moved the env-pure mutators ahead of
it and #7480 removed the per-call `clone_env`.

## Current state (2026-09-07, fourth pass -- five callgrind-driven slices)

The per-assertion budget was attacked again with callgrind only (the box has
no `perf`), on the protocol below: 300 `ok 1, "x"` under `MUTSU_REAL_TEST=1`
minus the one-assertion baseline, release build. Every slice landed as its
own PR with the deterministic before/after in its description; nothing was
special-cased for `Test`.

| slice | per assertion | PR |
| --- | --- | --- |
| start of the session | 492,188 Ir | -- |
| a literal parameter default binds directly (`proclaim`'s `$unescaped-prefix = ''` was an `eval_block_value` per call) | 454,016 | #7457 |
| named-call path: `Symbol == &str` compares in the return merge, per-call re-interning, `int`/`num`/`str` in the tag fast-accept, `is_native_method` before the numeric-bridge probes, byte-scan `function_key_base_name` | 397,305 | #7459 |
| free-variable reads stop cloning `current_package()`; `PackageKeyed<V>` (`FxHashMap`) for the unit/package lexical stores | 378,452 | #7460 |
| `pop_caller_env_with_writeback` skipped when nothing is `is dynamic`; `Mu` accepted up front; `__mutsu_type::` meta keys memoized as symbols | 352,748 | #7463 |
| a multi's dispatch-frame candidate list memoized per (name, package, lexical package, registry generation); `Env::flattened` clones a chain's root once | 333,577 | #7465 |

**-32.2% in instructions per assertion.** Wall clock on this box (which is
~2x the reference machine: `write-int.t` under the real module took 13.8 s
here at the start of the session against the 27.4 s the third pass
recorded): the 20,000-assertion `ok` loop 1.33 s -> 0.86 s,
`roast/S03-buf/write-int.t` under the real module 13.8 s -> 9.6-9.9 s
(median of three). Wall clock moved less than the instruction count because
`write-int.t`'s assertions carry Buf work the `ok` loop does not.

### What the profile looks like now (per assertion, 333.6k)

Inclusive, after #7465; the rows are what a sixth slice would have to take
on, and each is a structural change rather than a local one:

| row | Ir | what it is |
| --- | --- | --- |
| `bind_function_args_values` | 41.3k | the general binder: `filtered_args`/`plain_args` clones, the `@_` array per call, `check_and_coerce_param_type` for `Bool(Mu)` (6.4k), `type_matches_value` (7.2k), `bind_param_type_constraint` |
| `exec_call_method_mut_op` | 37.0k | `$output.say` through the user path (`dispatch_compiled_method_mut_with_raw_invocant` 11.3k + `try_native_io_handle_output` 9.8k) and `$desc.Str`; the scoped-env flatten is 8.8k of it now |
| free-variable reads/writes | ~30k | `unit_lexical_slot` 12.3k + `get_env_with_main_alias` 11.5k + `package_scope_lexical` 5.9k: ~12 reads of `$num_of_tests_run`, `$indents`, `$output`, ... at ~2.5k each, every one re-resolving the package chain |
| the `callframe().code` Sub | ~20k | `Value::make_sub(.., cf.params.clone(), cf.param_defs.clone(), .., clone_env())` in `call_compiled_function_named_inner`, twice per assertion: `Vec<ParamDef>` clone (`String::clone` 2.9k + `Vec::clone` 4.9k), `drop_in_place<Gc<SubData>>` 7.7k. Sharing `SubData.params`/`param_defs` behind an `Arc` touches ~315 `.param_defs` sites; a lazily materialized block-stack entry needs the registry key of the running candidate. Either is a real refactor |
| `Symbol::intern` | 21.3k | ~195 interns per assertion, diffuse: `type_meta_key_sym` (15), the binder (7), `native_lever_a_user_override` (6), `fn_keys_for_base` (11), `exec_set_local_op_inner` (8), `user_method_overloads` (7) |
| `exec_atomic_compound_var_op` + `store_named_scalar_rmw_result` | 17.6k | ONE `$num_of_tests_run = $num_of_tests_run + 1`: the read resolves through four fallbacks to the unit-lexical cell, then the store re-resolves it |
| `exec_set_local_op` | 15.9k | four `SetLocal`s (`my $tap`, `$tap ~=` x2, `my $ok`) at ~4k each |
| `Env::flattened` + env drops | ~27k | 15.9k flatten (one whole-scope clone per assertion, for `$output.say`) + `drop_in_place<Env>` 11.7k. The relocation attempt below is still the record on why this is not a local fix |
| `call_nqp_op` + `normalize_call_args_for_target` | 13.3k | five `nqp::` ops per assertion, each building a `Vec<Value>` |

The env flatten remains the one item this ticket has measured twice and
not solved; #7465's single-pass chain flatten took the cost of *being*
scoped two frames deep, not the cost of the flatten itself.

### Sweeps (2026-09-07, after #7465)

Both sweeps were re-run on the binaries carrying all five slices (#7465's
tree; #7457-#7463 had merged by then).

Roast sweep (release, whitelist, 3 jobs):

```
pass under both:                   1432
regressed under the real Test:     0
passes only under the real Test:   0
fail under both (pre-existing):    4
```

**`roast/S03-buf/write-int.t` no longer times out**: this is the first sweep
since the real-module mode was opened that reports zero regressions on the
roast side. The four fail-under-both rows are the same environmental four
as every earlier sweep (`6.c/S32-io/file-tests.t`,
`S10-packages/precompilation.t`, `S16-filehandles/filetest.t`,
`S32-io/IO-Socket-Async.t`).

`t/` sweep (debug, all 3775 files, 4 jobs):

```
pass under both:                   3753
regressed under the real Test:     0
passes only under the real Test:   0
fail under both (pre-existing):    22
```

### Completion criterion 2, re-measured under contention

The third pass warned that a sweep going green on an idle box does not close
the timeout class, and asked for a measurement on the reference machine and
under `prove -j4`. The reference machine is not available from this session;
the contention half was measured here. `write-int.t` under the real module,
run through `scripts/run-roast-test.sh` with `prove -j4 --timer` alongside
the five next-heaviest real-`Test` files (`S03-buf/read-write-bits.t`,
`S32-str/sprintf{,-b,-e,-x}.t`):

| condition | this box | scaled to the reference machine (x2) |
| --- | --- | --- |
| idle, median of three | 9.6-9.9 s | ~19.5 s |
| `prove -j4`, six heavy files | 13.4 s | ~27 s |

The idle margin against the 30 s budget went from ~9% (third pass) to ~35%.
Under `-j4` contention the scaled figure is still only ~10% under the budget,
so criterion 2 is **closer but not met**: `make roast` runs `-j4` on the CI
runner, and one more slice of the size of #7459 (-12%) would be needed
before the contended figure has the margin the idle one has. The rows in the
profile table above are where that slice has to come from; none of them is a
local fix any more.

Criterion 1 (no real-provider correctness regressions in either sweep) holds
as of this pass. Criterion 3 (`Test::Util` still composes) is exercised by
the roast sweep, which loads it from `roast/packages/Test-Helpers/` under both
providers.

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
**That file was measured on 2026-09-06 and its cost estimate was wrong**: the
carrier arm is 1.2% of the run, and the profile's real 8.5% item (a
registry-wide multi-candidate walk per dispatch) is now fixed, taking ~7.5% off
the 20 000-assertion `ok` loop. Read its corrected budget table before picking
up any of this.

`todo/perf/defaulted-param-forfeits-the-light-call-path.md` is also closed
(`news/2026-09/defaulted-params-reach-the-positional-light-path.md`), and its
`Test`-module claim is measured stale too: constant defaults now fill from a
registration-time table, but `ok` is a `multi` (multi names are excluded from
the name-keyed cache by construction) and `proclaim` carries `is copy` and a
`Bool(Mu)` coercion, so neither becomes light-eligible. `write-int.t` reports the
same 22 805 full resolves before and after -- the 565 212 that ticket quoted had
already been cut by the multi-resolve-cache fix. Ordinary defaulted subs gained
13.8x; this file gained nothing.

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
   regressions. **(Met 2026-09-07: `prove -j4 t/` all green, `make roast`
   identical to the native baseline.)**
2. The timeout class is eliminated or has a separately agreed test-budget
   solution; it must not merely be ignored by the sweep. **(Met on this box;
   the job-level budget needs `timeout-minutes: 45` on gc-stress/jit-stress.)**
3. `Test::Util` still composes with the default provider. **(Met -- the roast
   run loads it from `roast/packages/Test-Helpers/` throughout.)**
4. **The bundled-library gate (`scripts/battery-testsuite.sh`) passes under the
   vendored module.** It is a CI step and the sweeps do not cover it. **NOT met,
   but down from 9 regressing rows to 3** — see the sixth-pass section above for
   the three that remain and what each one actually is.
5. Run the focused tests, then `make test` and the relevant roast checks. The
   first default-provider PR must be treated as a full-suite review.

After the switch, remove the native `Test` interception and its maintenance
burden rather than retaining two production providers indefinitely.
