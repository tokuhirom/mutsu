# Make the vendored upstream `Test` module the default provider

The vendored upstream `Test.rakumod` (`modules/Rakudo-Core/lib/`, source hash
`f34dec45d52ad099c37f42fdbd93e277`, 953 lines of ordinary Raku) **runs verbatim
under mutsu**. What is still open is making it what a bare `use Test` resolves
to. Today that is opt-in behind `MUTSU_REAL_TEST=1`; the native TAP provider in
`runtime/test_functions.rs` is still the default.

The flip was attempted on 2026-09-07/08 and **withdrawn** — see "Why it was
withdrawn" below. This file is the entry point for resuming it.

## What is already done

The campaign's real product was never the flip: it was the ~130 general
interpreter fixes the module forced along the way. The module exercises phasers,
`EVAL`, `callframe`, custom traits, `nqp::` ops, subtests and lazy evaluation, so
every gap it hit was a gap in mutsu, not in `Test`. Those fixes are all landed
and are independent of which provider is default. This is BATTERIES.md rung 2 in
its intended form — grow the interpreter until the real module runs verbatim,
rather than reimplement it natively.

## The completion criteria, and where they stand

Measured on a 4-core box, 2026-09-07/08, release build unless stated.

**1. No correctness regression in the roast and `t/` suites — MET.** The roast
dual-provider sweep (`scripts/roast-test-module-sweep.sh`, whitelist, both
providers, 4 jobs) ended at 1431 pass under both, 0 regressed, 0 pass-only-under-
real, 4 fail under both (container-environment files). Under the vendored module
as default: `prove -j4 t/` 3836 files / 40528 tests PASS, and `make roast` 1436
files / 218939 tests with only the known container-environment failures.

**2. The per-file timeout class is closed — MET.** `roast/S03-buf/write-int.t`
(~93 000 assertions) against the 30 s budget: 11.4 s idle, 14.6 s under
`prove -j4` alongside the five next-heaviest files — a 51% margin, measured
directly on 4 cores rather than scaled from a 12-core box.

**3. `Test::Util` still composes — MET.** roast loads it from
`roast/packages/Test-Helpers/`, exercised by the sweep under both providers.

**4. The `Bundled-library test suites` gate — NOT MET.** This is the blocker.

## Why it was withdrawn

The gate (`scripts/battery-testsuite.sh`, run inside CI's `test` job) reports
**282/312 with 9 regressed whitelisted files** under the vendored default. They
are four unrelated interpreter gaps plus five `DBIish` rows that need live
database servers, none of them a `Test` compatibility problem — the same pattern
as every other fix in this campaign, except that these did not have fixes small
enough to ride along with the flip.

The root causes, with the reductions already done, are in
**`todo/deep/vendored-test-battery-gate-regressions.md`**. Read that before
resuming. In particular the `NativeLibs` one *was* fixed in a one-line change and
the fix had to be reverted: it is gated on multi-dispatch introspection
(`&trait_mod:<is>.candidates` / `.signature` / `.dispatcher`) and without that it
converts a quiet wrong answer into a hard error, taking DBIish's `install-driver`
down with it.

Landing the flip while the gate is red would mean re-baselining the gate's
whitelist, which hides exactly the compatibility regression the gate exists to
catch. That is not a step to take to get a green check; it needs a maintainer
decision, and the ordinary answer is to fix the four root causes first.

## Suite cost of the flip, when it happens

| | native | vendored |
| --- | --- | --- |
| `make roast` (release, whitelist) | 250 s | 309 s (1.24x) |
| `prove -j4 t/` (release) | ~55 s | ~110 s (2.0x) |

`make roast` pays little because most whitelisted files are not assertion-bound.
The `t/` doubling is the standing CI cost of running Raku's own `Test` as Raku
code; five callgrind passes already took the per-assertion cost from 492k to
235k instructions, and `todo/perf/interpreter-call-path-in-hot-loops.md` is the
remaining lever.

## Harness details the flip has to settle again

These were solved during the attempt and are worth not re-deriving. Two of the
three are already landed and provider-conditional, so they cost nothing today:

- **`#?rakudo todo`** (landed). The preprocessor emitted
  `todo '__mutsu_backend_todo__:<reason>'`, a marker asking the *native* provider
  to drop the `# TODO` annotation when the assertion passes (the directive speaks
  about rakudo; mutsu is a different backend). The upstream module has no such
  convention and would leak the marker into the TAP description, so under the
  vendored provider the bare reason is emitted and rakudo's own reporting is
  accepted. `make roast` then lists more "TODO passed" rows; they are not
  failures.
- **Bundled modules inside `cargo test`** (landed, and a general fix).
  `resolve_bundled_lib_paths` had no candidate for `target/<profile>/deps/`,
  where a cargo test binary lives, so every `use` of a bundled battery inside a
  `#[test]` failed with "Could not find Test in: (module repositories)" the
  moment `Test` became one.
- **`t/is-deeply-user-raku-diagnostic.t`** (still to do at flip time). It asserts
  its `expected:`/`got:` diagnostic on STDOUT, which is what the native provider
  emits. rakudo and the vendored module both send a non-TODO failure's diagnostic
  to `$failure_output` (STDERR) — running the file under `raku` itself reproduces
  the split. Flip those two assertions to `:err` with the default; the file
  carries a comment saying so. It is the only `t/` file whose expectation differs
  between the providers.

## After the flip

Deleting the native provider — ~3600 lines across `src/runtime/test_functions/`,
`src/runtime/subtest.rs`, `src/vm/vm_native_test.rs`, the
`real_test_module_enabled()` gate at its call sites, the
`__mutsu_backend_todo__` marker, and the `MUTSU_REAL_TEST` escape hatch with its
two sweep scripts — is tracked separately in
`todo/deep/retire-the-native-test-provider.md`.
