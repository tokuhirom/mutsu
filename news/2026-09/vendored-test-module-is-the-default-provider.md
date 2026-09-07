# The vendored upstream `Test` module is the default provider

`use Test` now loads the unmodified upstream `Test.rakumod` vendored at
`modules/Rakudo-Core/lib/Test.rakumod` (source hash
`f34dec45d52ad099c37f42fdbd93e277`). mutsu's native TAP provider is no longer
what a `use Test` resolves to; it survives only behind `MUTSU_REAL_TEST=0`, so
the dual-provider sweeps can keep comparing the two while it is retired.

This closes `todo/deep/vendor-real-test-module.md`, a campaign that ran from
2026-08 and whose real product was not the switch but the ~130 general
interpreter fixes it forced along the way — the module is 953 lines of ordinary
Raku exercising phasers, `EVAL`, `callframe`, custom traits, `nqp::` ops,
subtests and lazy evaluation, so every gap it hit was a gap in mutsu, not in
`Test`. This is BATTERIES.md rung 2 in its intended form: grow the interpreter
until the real module runs verbatim, rather than reimplement it natively.

## The completion criteria, and how they were met

The ticket refused the switch until four things held. All four were measured on
a 4-core box on 2026-09-07, release build unless stated.

**1. No correctness regression under the real provider.** The roast sweep
(`scripts/roast-test-module-sweep.sh`, whitelist, both providers, 4 jobs)
opened the session at:

```
pass under both:                   1431
regressed under the real Test:     1
passes only under the real Test:   0
fail under both (pre-existing):    4
```

The single regression, `roast/integration/99problems-21-to-30.t`, turned out
not to be a `Test` problem at all: an `is rw` writeback queued for a *slice*
subscript argument fired against the caller's immutable List once the vendored
module's `is` reified a deferred `map` Seq later than the native provider did.
Fixed as a general argument-passing bug —
`news/2026-09/rw-slice-arg-is-not-a-container.md`. `make test` and `make roast`
then found one more, in the opposite direction: ADR-0058 step 3b's deferred
`.grep` clobbered its own consuming frame's lexicals, visible only once
`reflective_name_access_possible()` had latched, which every file that loads the
real `Test` does — `news/2026-09/grep-capture-merge-keeps-caller-lexicals.md`.
Neither was a `Test` compatibility shim; both were interpreter bugs the module
merely exposed, which is the pattern the whole campaign followed.

With both fixed, on the default (vendored) provider:

- `make test` — 3810 files, 40177 tests, **PASS**.
- `make roast` — 1436 files, 218939 tests, one failure:
  `roast/S32-io/IO-Socket-Async.t` (exit 124), one of the four
  container-environment files that fail under *both* providers here. The
  native-provider baseline run on the same box failed three of those four, so
  the vendored provider is not behind on this box.
- `cargo test --lib` — 955 passed.

**2. The timeout class is closed.** The heaviest real-`Test` file,
`roast/S03-buf/write-int.t` (~93 000 assertions), against the 30 s per-file
budget:

| condition | native | vendored |
| --- | --- | --- |
| idle, median of three | 3.8 s | 11.4 s |
| under `prove -j4` with the five next-heaviest files | -- | 14.6 s |

A 51% margin under contention on a 4-core box — the same shape as a CI runner —
where the fifth optimization pass had only been able to *estimate* ~22 s by
scaling from a 12-core machine. Whole-suite cost of the switch:

| | native | vendored |
| --- | --- | --- |
| `make test` (debug binary, `t/`) | 542 s | 1097 s |
| `make roast` (release binary, whitelist) | 250 s | 309 s |

`make roast` pays only 1.24x because most whitelisted files are not
assertion-bound; `make test` pays 2.0x. That is the standing CI cost of running
Raku's own `Test` as Raku code, and it is what five passes of callgrind-driven
optimization (492k -> 235k instructions per assertion, recorded in the ticket's
history) bought down to affordable.

**3. `Test::Util` still composes.** Roast loads it from
`roast/packages/Test-Helpers/`, and the roast sweep exercises it under both
providers.

**4. Full-suite review.** The three suites above, plus `cargo clippy
--all-targets -D warnings` and `cargo fmt`.

## Two harness details the switch had to settle

**`#?rakudo todo` reporting.** The roast preprocessor turned a `#?rakudo todo`
into `todo '__mutsu_backend_todo__:<reason>'`, a marker asking the *native*
provider to drop the `# TODO` annotation when the assertion actually passes:
the directive says the test is expected to fail on rakudo, and mutsu is a
different backend, so a pass here is not the "unexpectedly succeeded" event the
annotation reports. The upstream module — the one rakudo itself ships — has no
such convention and would have leaked the marker verbatim into the TAP
description. Under the vendored provider the preprocessor now emits the bare
reason and accepts rakudo's own reporting, so `make roast`'s summary lists more
"TODO passed" rows than before. They are not failures.

**Bundled modules inside `cargo test`.** `resolve_bundled_lib_paths` discovered
the bundle relative to the running binary and had no candidate for
`target/<profile>/deps/`, where a cargo test binary lives — so every `use` of a
bundled battery inside a `#[test]` failed with "Could not find Test in: (module
repositories)" the moment `Test` became one. The `deps` layout is now
recognized, guarded on the directory actually being named `deps` so an
installed layout cannot reach a stray `modules/` three levels up.

**`t/is-deeply-user-raku-diagnostic.t`** asserted its `expected:`/`got:`
diagnostic on STDOUT. Upstream `Test` sends a non-TODO failure's diagnostic to
`$failure_output` (STDERR); running the file under `raku` itself reproduces the
same split, so the local test was corrected rather than the interpreter. It now
passes under `raku` and under the vendored provider, and fails under the native
one — whose diagnostic omits those lines entirely. That is the first `t/` file
to pin behaviour the native provider does not have, and it is deliberate.

## What is left

Retiring the native provider itself: ~3600 lines across
`src/runtime/test_functions/`, `src/runtime/subtest.rs` and
`src/vm/vm_native_test.rs`, the `real_test_module_enabled()` gate at its seven
call sites, the `__mutsu_backend_todo__` marker, and the `MUTSU_REAL_TEST`
escape hatch with the two sweep scripts that use it. Keeping two production
providers indefinitely is the thing this campaign existed to end; the escape
hatch is a transition aid, not a supported configuration.
