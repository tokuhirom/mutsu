# A loop-redeclared lexical mutated by a `cue` callback keeps its previous iteration's value

## Minimal repro

```raku
for 1..6 -> $round {
    my $a = 0;
    my $c = $*SCHEDULER.cue({ cas $a, {.succ} }, :every(0.1));
    sleep 1;
    $c.cancel;
    say "round $round: $a";   # expected ~10 every round
}
```

mutsu prints `10, 20, 30, 41, 52, 62` — each round's `$a` starts from the
previous round's final value instead of 0. raku prints ~10 every round. The
same shape with `start` instead of `cue` is correct (`1, 1, 1` for
`for 1..3 { my $a = 0; await start { cas $a, {.succ} }; say $a }`).

**This is NOT an ADR-0020 regression**: it reproduces identically on the
pre-pool thread-per-cue implementation (verified on main `2d6db20c4`, which
prints `10, 20, ..., 60`).

## Root cause (as far as traced, 2026-08-05)

The `cas $a` in the cue callback does not go through the name-keyed shared
lane (`shared_vars` stays `0` throughout, observed with debug tracing) — it
goes through the bare-name atomic key machinery (`reset_atomic_var_key_decl` /
`__mutsu_atomic…`), and for a *loop-body* lexical the redeclaration reset does
not stick: the next iteration's `my $a = 0` runs `reset_atomic_var_key_decl`,
but the callback thread's writes re-create/overwrite the bare-name entry, and
the loop iteration's fresh `$a` resolves back to it. Block-scoped versions
(`{ my $a = 0; ... }` sequences) are mostly unaffected because each block's
decl + `thread_redeclared_vars` mask + `clone_for_thread`'s `declare` reseeds
the lane; the loop-body slot path skips part of that (the loop lexical lives
in a slot, `env.get("a")` at cue time shows the right `0`, yet the callback
still resolves the stale atomic entry).

Related mechanism notes: `clone_for_thread_excluding` (ADR-0010 seeding),
`vm_var_assign_set_local.rs` (`thread_redeclared_vars` /
`reset_atomic_var_key_decl`), and the `t/lock.t` fix (#4167) which moved
shared-array pushes to the `__mutsu_atomic_arr::` store — the scalar `cas`
lane has the same bare-name-cannot-represent-two-bindings limitation.

## Why it is not fixed inline

The fix is in the bare-name atomic lane's interaction with loop-redeclared
lexicals, not in the scheduler: any callback-holding construct that outlives a
loop iteration (a `cue`, a tap, a timer) can resurrect the entry. It needs the
same per-binding-cell treatment `start` blocks got via
`box_captured_lexicals`/`clone_for_thread_for_block` (which `cue` now uses,
2026-08-05 — insufficient because the atomic lane is separate), or the atomic
key needs a binding-generation component. That is ADR-0010/Track-B-adjacent
design work.

## Impact

- `:every` cues (and plausibly taps/timers) whose callbacks `cas`/atomically
  mutate a loop-body lexical accumulate across iterations.
- The `.cancel` bounded wait added in ADR-0020 slice 2 hides the common roast
  shape (block-sequenced cues, `LEAVE $c.cancel`), so
  `roast/S17-scheduler/every.t` is stable — but the loop shape above remains
  wrong deterministically.
