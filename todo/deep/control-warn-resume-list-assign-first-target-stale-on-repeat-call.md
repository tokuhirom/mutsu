# A `CONTROL { when CX::Warn { ...; .resume } }` sub's list-assigned first return value goes stale on the second+ call, but only after `use Test;` loads the real vendored module

Found continuing `todo/deep/vendor-real-test-module.md` (2026-08-18), investigating
why `t/warn-resumes-at-the-raise-site.t` regresses under `MUTSU_REAL_TEST=1`.
Not a `Test`-shape problem and not related to the topic-corruption bug fixed in
`news/2026-08/bind-topic-does-not-splice-into-ancestor-frames.md` — it is a
distinct, general bug in how a resumed warning's local-variable writes survive
into a subsequent multi-value list assignment.

## Minimal repro

```raku
use Test;

sub f(&code) {
    my ($x, $y, $z) = False, '', False;
    code();
    $z = True;
    CONTROL { when CX::Warn { $x = True; $y = .message; .resume } }
    ($x, $y, $z);
}

my ($x, $y, $z) = f({ warn "boom" });
say "first: x=$x y=$y z=$z";

($x, $y, $z) = f({ warn "boom2" });
say "second: x=$x y=$y z=$z";
```

```
$ MUTSU_REAL_TEST=1 target/debug/mutsu repro.raku
first: x=True y=boom z=True
second: x=False y=boom2 z=True
```

`raku` (and mutsu without `MUTSU_REAL_TEST=1`, i.e. `use Test;` intercepted by
the native provider) both print `x=True` on both calls. `$x` — and *only*
`$x` — reverts to its function-entry default (`False`) on the second call,
even though the `CONTROL` handler visibly ran (`$y` picked up `"boom2"`,
`.resume` did resume execution, `$z`'s pre-existing `True` survived).

## What was ruled out

- **Not the `:=`-bind topic-splice bug** (already fixed) — this repro has no
  `:=` bind at all.
- **Not about the variable name.** Renaming `$did`/`$msg`/`$reached` to
  `$x`/`$y`/`$z` (or anything else) reproduces identically — it is not a
  bare-name collision with something inside `Test.rakumod`.
- **Not about which local slot declares first.** Reordering the `my (...)`
  declaration inside `f` (`$msg, $reached, $did` instead of `$did, $msg,
  $reached`) does not change which one breaks.
- **It is specifically the FIRST position of the *caller's* multi-value
  re-assignment** that goes stale, regardless of which of `f`'s locals maps
  there: permuting the caller's `($z, $y, $x) = f(...)` (return order inside
  `f` still `($x, $y, $z)`) moved the failure to whichever caller variable
  occupies LHS position 0.
- **Not JIT.** Reproduces identically with `MUTSU_JIT=off`.
- **Not "any module load."** An empty user module (`unit module Empty; sub
  noop() is export {}`, loaded via `-I` and `use Empty;`) does NOT trigger
  it — the same `f`/caller shape works correctly on every call. It requires
  loading the real, large `Test.rakumod` specifically (`use Test;` under
  `MUTSU_REAL_TEST=1`); grepping the vendored module for `CONTROL` finds zero
  matches, so it is not that `Test.rakumod` itself declares a colliding
  `CONTROL` handler.
- **Not a general "list-assign to already-declared scalars, called twice"
  bug** — a `CONTROL`-free `sub f($n) { (100+$n, 200+$n, 300+$n) }` called
  three times with the exact same caller-side re-assignment pattern returns
  correct values every time, with or without `use Test;`. The bug needs the
  `CONTROL { when CX::Warn { ...; .resume } }` + a nested-closure-raised
  `warn` specifically.

## What is not yet known

- Whether it is really "the module is large" (many declared subs shifting
  some global counter/ID the resume mechanism depends on — `state_scope_id`,
  a callable-id allocation, `control_handler_depth`/`control_handlers` stack
  bookkeeping, or similar) or something else about `Test.rakumod`'s content
  specifically. A same-size empty/synthetic module was not tried — the next
  step is generating a large synthetic module (many `sub`s, no real bodies)
  and seeing whether *that* alone reproduces it, which would confirm the
  "large module shifts an ID" theory and point at exactly which counter.
- Whether `.resume`'s "jump back into the raise site" implementation reads a
  local's value from a stale pre-CONTROL-handler snapshot for *only* the
  first slot in the caller's assignment target list, or whether the bug is on
  the *write* side inside `f` (the `$x = True;` write inside the `CONTROL`
  block itself gets lost / never lands in the slot the later `($x, $y, $z)`
  return-tuple construction reads from) — `rust-gdb` breakpoints on the
  resume-ip handling in `vm_run_loop.rs`/`vm_try_catch_ops.rs`
  (`take_resume_ip_for`, the JIT warn-resume shim) and on the list-assignment
  opcode are the next concrete step, watching the local slot for `$x`
  specifically across the second call.

## What it blocks

`t/warn-resumes-at-the-raise-site.t` under `MUTSU_REAL_TEST=1`
(`todo/deep/vendor-real-test-module.md`'s residue list). Not investigated
against roast directly yet — worth a targeted grep for roast files combining
`CONTROL`/resumable `warn` with a multi-value list-assigned caller once this
is root-caused, since the same mechanism would corrupt any of them the same
way.
