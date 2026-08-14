# A `t/`-resident file's second+ `is_run` call involving `$*RAKU`/`use v6.x` never spawns its child

Found 2026-08-14 while writing a regression test for
`todo/tickets/magic-vars-should-be-built-lazily.md` Slice 2 (lazy
materialization of `$*DISTRO`/`$*PERL`/`$*RAKU`/`$*VM`/`$*KERNEL`).

## Symptom

In a test file that lives under `t/` (not elsewhere — see below) and `use`s
`Test::Util` (`roast/packages/Test-Helpers/lib/Test/Util.rakumod`), calling
`is_run` more than once with code that both reads `$*RAKU` and sets an
explicit non-default `use v6.x` makes the **second and later** calls fail —
not with a wrong-value mismatch, but because the spawned child process is
never even started. Minimal repro (as `t/repro.t`):

```raku
use Test;
use lib $*PROGRAM.parent(1).add("roast/packages/Test-Helpers");
use Test::Util;

plan 3;
is_run 'use v6.c; print $*RAKU.version', { :out<6.c> }, 'first';   # ok
is_run 'use v6.e; print $*RAKU.version', { :out<6.e> }, 'second';  # not ok
is_run 'print $*RAKU.version',           { :out<6.d> }, 'third';   # not ok
```

`strace -f -e trace=execve,clone3` across the whole run shows only the
parent's own `execve` and its VM-worker `clone3` — no second `execve` for a
child `mutsu` process at all, for calls 2 and 3. Since `get_out`
(`Test::Util.rakumod`) wraps its temp-file-write + `shell()` invocation in a
`try { ... } CATCH { %out<test_died> = ~$! }`, and `is_run` checks
`%got<test_died>` before falling through, the natural guess is an exception
inside that `try` block before `shell()` is ever reached — but `%got<test_died>`
must be reading as falsy too, since `is_run` reports a plain `not ok N` (via
its own `ok ?$ok, $name;`) with **zero** `diag` lines, not the `skip 'test
died: ...'` path that a truthy `test_died` would take. That is inconsistent
with the loop that builds `@diag_q` (it unconditionally pushes onto a
`!$attr_good` mismatch), so either an exception happens inside that loop
itself (aborted before any `diag` line prints) or something more surprising
is going on with `%expected`/`%got`'s container identity across multi-dispatch
delegation. Not fully root-caused — see "What's ruled out" below for what is
known NOT to explain it.

## Reproduction is oddly specific — read this before assuming a broad fix is needed

- **Directory matters**: identical content in `t/*.t` fails; the exact same
  file copied to `tmp/*.t` (one call to `get_out`/`is_run`, two, or three; with
  or without other `$*DISTRO`/`$*PERL`/`$*RAKU`/`$*VM`/`$*KERNEL` reads before
  it) always passes. `$*PROGRAM.parent(N)` was adjusted so `use lib` resolves
  to the identical absolute `roast/packages/Test-Helpers` path in both
  locations — the failure is about **which directory the running file itself
  lives in**, not about where `Test::Util` is loaded from.
- **Call count matters**: a single `is_run` call with `$*RAKU`+`use v6.x`
  content in `t/` succeeds; the second (and every later) one in the same file
  fails, even when the first call's content is unrelated (e.g. `is_run 'print
  "one"', { :out<one> }`) — no, wait: unrelated content for ALL calls (no
  `$*RAKU` anywhere) passes every time, so it needs both (a) 2+ calls and (b)
  at least the failing call(s) referencing `$*RAKU`.
- **Whether the OUTER file itself already read `$*RAKU`/`$*PERL` before the
  first `is_run` call does NOT matter** — moving such a read before vs. after,
  or removing it outright, does not change whether call #2 fails. (An earlier,
  wrong theory in this investigation blamed the lazy-materialization change
  itself for this — it does not reproduce with `$*RAKU` never read directly,
  only referenced inside the `is_run` code string.)
- Calling `get_out` directly (not through `is_run`) with the exact same
  arguments, preceded by the exact same assertions, always returns the
  correct `%got` (verified via `note`) — so the bug is specific to `is_run`'s
  own dispatch/body, not to `get_out`/`shell()`/temp-file handling in
  isolation.

## What's ruled out

- **Not the lazy-materialization change itself.** The pre-fix (eager
  construction) baseline binary does NOT reproduce this — `git stash`-ing this
  ticket's Slice 2 changes and rebuilding makes the repro pass. So this is a
  genuine regression surfaced by Slice 2, but the mechanism is not "$*RAKU
  resolves to the wrong value" (the child, run standalone with the exact same
  code, always prints the correct version) or "$*RAKU is read and that read
  fails" (a bare `$*RAKU.defined` read before a single `is_run` call is fine).
  Something about the *timing/allocation pattern* Slice 2 introduces exposes a
  pre-existing fragility elsewhere; it isn't a defect in Slice 2's own logic
  (env-lookup fallback, `OnceLock` caching, `make_perl_instance` reading
  `current_language_version()` at construction time).
- **Not stale precompilation cache.** `rm -rf ~/.cache/mutsu/precomp/*`
  immediately before the repro does not change the outcome (reproduces with a
  cold cache every time).
- **Not `roast/`-specific special-casing** — copying
  `roast/packages/Test-Helpers` verbatim to a scratch directory with its
  `META6.json` intact, loaded via `use lib` from a `t/`-resident file,
  reproduces the SAME failure as loading the real `roast/packages/...` copy —
  so it is not about the module's specific file identity, only about the
  *caller's* directory.
- **Not a `MONKEY-GUTS` resolution difference** — `MONKEY-GUTS` has no
  corresponding `.rakumod` anywhere in the tree; it is a built-in
  pragma-like `use`, unaffected by search path.

## Suspects not yet fully explored

- `src/runtime/run_modules.rs`'s automatic module-search fallback: besides an
  explicit `use lib`, module resolution ALSO tries
  `{ancestor}/roast/packages/{top}[-Helpers]/lib/{file}` for every ancestor of
  `$*PROGRAM`'s path (`candidates.push(ancestor.join("roast")...)`,
  `run_modules.rs:81-88`). For a `t/`-resident file this generates a
  **duplicate** candidate identical to the explicit `use lib` path; for a
  file elsewhere it may not (depending on ancestor depth), or may point
  somewhere that does not exist. If module resolution or the resulting
  `is test-assertion`-tagged multi's registration is not perfectly
  idempotent under a duplicate/repeat resolution, a second load could produce
  a second, distinct `Test::Util` symbol table with its own (possibly
  differently-cached) `is_run` multi candidates — and stale/duplicate
  multi-dispatch resolution cache entries (ADR-0019's generation-keyed
  resolved-sequence cache, or the carrier-compile-cache landed the same day
  as this investigation, see `perf(carrier-compile-cache)` on `main`) could
  then pick the wrong one under a timing change. This is a plausible
  mechanism but NOT verified — the actual duplicate-registration step was not
  isolated.
- `is test-assertion`'s caller-frame-walking machinery for reporting
  `at file line N` (`src/runtime/call_helpers.rs`, `src/vm/vm_call_func_ops.rs`)
  was flagged as fragile in the immediately preceding session
  (`session-test-assertion-trait-mixin-callframe`, 2026-08-14, PR #6388) —
  worth checking whether a second `is_run` invocation's frame walk interacts
  badly with the first's already-consumed frame state specifically when the
  caller file sits directly under `t/`.

## Why this is filed instead of fixed here

The combination needed to trigger it (a `t/`-resident file, 2+ `is_run`
calls, at least one referencing `$*RAKU` under a non-default `use v6.x`) does
not occur anywhere in the current roast whitelist or `t/` suite — confirmed by
a full `make test` run on the branch that discovered this, which is clean.
Root-causing it needs to distinguish between at least two different
subsystems (module resolution duplication vs. `is test-assertion` callframe
handling vs. multi-dispatch caching) that are all orthogonal to
`todo/tickets/magic-vars-should-be-built-lazily.md`'s actual scope (env-lookup
fallback for 5 magic vars). The property it would have pinned in `t/` is
instead pinned as a Rust unit test
(`make_perl_instance_version_reflects_current_language_version` in
`src/runtime/io_sysinfo.rs`), which exercises the exact same code path
(`Interpreter::make_perl_instance`) with no subprocess involved.

## Repro checklist for whoever picks this up

1. Reproduce with the minimal file above (`t/repro.t`), confirm still current.
2. `strace -f -e trace=execve,clone3` to confirm no second `execve`/`clone3`
   for a `shell()`-spawned child on the failing calls.
3. Instrument a scratch copy of `Test::Util.rakumod` (loaded via a **literal**
   `use lib 'tmp/scratch-path'` — NOT `$*PROGRAM.parent(N).add(...)`, whose
   `.parent(N)` depth is easy to get wrong for a file outside `t/`/`tmp/`) to
   add `note` calls inside `is_run`'s 4-arg candidate, and confirm whether
   they even fire for the failing call (they did not, in this investigation,
   for the *directly-under-`roast/`* real copy loaded from a `t/`-resident
   file — but DID fire, correctly, for a byte-identical copy under `tmp/`).
4. If they don't fire, the exception (or wrong-candidate dispatch) happens
   before `is_run`'s own body runs at all — focus on module
   resolution/registration duplication (`run_modules.rs`) and multi-dispatch
   candidate lookup, not on `get_out`/`shell()`.
