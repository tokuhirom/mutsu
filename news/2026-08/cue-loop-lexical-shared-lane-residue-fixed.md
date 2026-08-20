# A loop-redeclared lexical mutated by a `cue` callback no longer keeps the previous iteration's value

A loop shape like

```raku
for 1..6 -> $round {
    my $a = 0;
    my $c = $*SCHEDULER.cue({ cas $a, {.succ} }, :every(0.1));
    sleep 1;
    $c.cancel;
    say "round $round: $a";   # expected ~10 every round
}
```

used to print `10, 20, 30, 41, 52, 62` on mutsu instead of ~10 every round:
each round's `$a` started from the previous round's final value instead of
0, even though the loop redeclares `my $a = 0` fresh every iteration. The
same shape with bareword `start` instead of `$*SCHEDULER.cue` was already
correct, which pointed at something specific to method-call spawn paths
(`.cue`, `Promise.start`, `Thread.start`) rather than the bareword `start`
function.

## Root cause

`cas $a, ...` compiles to `__mutsu_cas_var("a", ...)` — the target variable
name becomes a string constant, invisible to the compiler's ordinary
free-variable scan. That scan is what decides which locals need their `env`
mirror kept current when a nested closure captures them (`env` only mirrors
a local's live value for slots something reads by NAME; a closure normally
reads its free variables straight from the creating frame's live local
slots, so a captured local does not force an env flush by default). Because
`cas`'s target was invisible to the scan, `my $a = 0`'s own declaration
never flushed `env["a"]`, and the redeclaration on the next loop iteration
reset only the local *slot* — not `env`, which stayed at whatever value a
*later* `sync_shared_vars_to_env` call had last written back from a
finished cue tick. When the next round's `cue(...)` cloned the interpreter
for its worker thread, the clone's `env` snapshot carried that stale value;
the round's first `cas` tick's `atomic_current_value` fallback
(`shared.get(value_key).or_else(|| self.env.get(name))`) found no entry yet
for the brand-new atomic key and fell through to the stale `env["a"]`,
so the round's count accumulated on top of the previous round's total.

Three attempts to fix this directly (blanket-syncing locals into env before
a thread spawn, in various narrowed forms) were tried and reverted in an
earlier session, each fixing the reported bug while introducing a different
regression (a socket-FD double-`dup()` hang in `Cro::Core/tcp.rakutest`, two
free-variable-analysis completeness gaps, and an unexplained `Nil`/`Any`
divergence surfaced only by a full `make test` run). See the git history of
this file (formerly `todo/deep/cue-loop-lexical-shared-lane-residue.md`) for
the detailed trace of each attempt.

## What actually fixed it

The bug was closed as a side effect of an unrelated fix, commit `3b5a0efc5`
("stop inline start-block spawns from clobbering a later-declared local"),
which addressed a sibling bug where a `start { ... }` block passed inline as
a call argument spawned before the variable it would be assigned to had its
real value. That fix added `CompiledCode::rw_arg_env_sync_syms`: a `cas`
target reached through a nested closure at *any* depth is now recorded and
bubbled up to the frame that actually declares the variable, where it is
folded into `needs_env_sync` alongside the existing atomic-target tracking.
Because this fold happens in `compute_needs_env_sync` — a general,
per-slot, compile-time analysis consulted by the ordinary per-store write
path — it fixes the env mirror for the local's *own* declaration/
redeclaration regardless of which call later spawns the worker thread
(`$*SCHEDULER.cue`, `Promise.start`, `Thread.start`, or bareword `start`
alike), not just the inline-`start`-argument case the commit's own
description focuses on.

Verified by re-running the original repro (10/10/10/... across 6 rounds,
stable across repeated runs) and by tracing the exact code path: `cas $a`
inside the cue callback closure is not a local of that closure's own frame,
so `note_atomic_env_sync_target` records `$a` in the closure's
`rw_arg_env_sync_syms`; `compute_free_vars` bubbles that name up to the
for-loop body's frame (where `$a` *is* a local); `compute_needs_env_sync`
then marks that local's slot `needs_env_sync = true`, so `my $a = 0`'s
declaration flushes `env["a"]` to 0 on every loop iteration, closing the
staleness window a freshly spawned worker thread's `env` snapshot could
otherwise inherit.

## Regression coverage

Added `t/cue-loop-lexical-reset.t`, which pins the exact loop-redeclaration
shape but uses `$*SCHEDULER.cue(..., :times(10))` instead of `:every` plus a
sampling window — `:times` makes the final count exact and deterministic
(no timing tolerance needed), so a reintroduced leak fails with an exact
multiple of 10 rather than a fuzzy threshold.
