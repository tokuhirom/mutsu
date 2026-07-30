# Recursion through a `start` block: retiring the name-keyed scalar lane for a block's own captures

Status: **landed** (2026-07-30, re-implemented from PR #4654's investigation on top of
ADR-0010's lineage-scoped shared store). This document records the bug, the final
design, and the load-bearing facts discovered on the way — the original PR #4654
branch predates ADR-0010 and was abandoned in favor of this re-implementation.

## The bug

```raku
sub f($n) { start { $n <= 0 ?? "b" !! await(f($n - 1)) ~ "|$n" } }
say await f(3);   # b|1|2|3   correct
say await f(3);   # b|3|3|3   WRONG, no error   (raku: b|1|2|3)

sub k($n) { $n <= 0 ?? "b" !! (await start { k($n - 1) }) ~ "|$n" }
say k(3);         # b|1|1|1   WRONG ON THE FIRST CALL (raku: b|1|2|3)
```

A two-branch `fib` (`await(fib($n-2)) + await(fib($n-1))` inside `start`) hung
deterministically on the second call. Plain recursion (no `start`) and
non-recursive `start` were both correct. Pin: `t/recursive-start-await.t`.

## Root cause

`clone_for_thread` seeded **every** lexical into the shared store, keyed by the
**bare name**. A bare-name map cannot represent two concurrently-live bindings of
one name — exactly what a recursive frame chain is. Each frame's `$n` overwrote
the one entry, and `await`'s `sync_shared_vars_to_env` +
`apply_pending_caller_var_writeback` (retain-on-miss, walks **up** the frame
chain) force-fed the innermost value into every ancestor frame's slot.

Crucially, `start` already compiles its block as escaping, so
`box_captured_lexicals` already gives each frame's captured scalar a correct
per-binding home — a shared `ContainerRef` cell when mutated, a frozen value when
read-only. The name lane was a second, lossy mechanism running in parallel with a
working one and overwriting its correct answer.

## The design that landed

1. **A spawned block's own captured plain scalars are not seeded by name.**
   `clone_for_thread_for_block` (used by `spawn_callable_promise`, i.e. `start`,
   `Promise.start`, `Thread.start`) skips them; the closure machinery owns them
   per binding. The exclusion is an ALLOW-list (`Int`/`Num`/`Str`/`Bool`/rats/
   `Complex`/`ContainerRef`): everything else (a `Channel`, `Lock`, `Promise`,
   aggregate, sub, type object) is a shape `box_captured_lexicals` declines to
   box, so the name lane is still what keeps parent and worker on ONE object
   (`t/concurrency-threading.t` test 4 hangs otherwise).
2. **Excluded names keep the re-declaration mask, on both sides of the spawn.**
   The parent-side `thread_redeclared_vars.clear()` at spawn rests on the premise
   "the current value was just force-seeded"; for excluded names it was not, so
   the mask is retained there, and the child starts with the excluded set as its
   mask. Without this, a stale store entry under the name — the blanket
   `sync_env_from_locals` mirror writes declared-but-not-yet-initialized slots
   (Nil) into the store, and the mask then blocks the post-assignment refresh —
   is pulled back over the live binding at the next `await`. That was
   `roast/S17-promise/nonblocking-await.t`'s socket test: `$port` went Nil, the
   connect went to port 0, and the whenever received the Failure string
   (PR #4654 left exactly this unresolved).
3. **Names written by a registered class/role method keep the name lane**
   (`CompiledCode::type_body_written_lexicals`, recorded at
   `RegisterClass`/`RegisterRole`). Such a method has no closure-creation op, so
   the capture analysis cannot see its writes; the lane is the only carrier for
   `my $a = 0; class Foo { submethod DESTROY { $a++ } }` on a worker
   (roast `S12-construction/roles-6e.t`,
   `t/destroy-cross-thread-writeback-coherence.t`).
4. **Typed scalars are boxed when the closure goes to a thread**
   (`CompiledCode::thread_escaping`, set for `start` args and `.start` method
   args). Boxing does not weaken the constraint — the check runs at the
   assignment op, looked up by name in `var_type_constraints` (cloned into the
   child), before any write-through. It must NOT happen for a same-frame closure:
   `cas` resolves its target by name (`roast/S17-lowlevel/cas.t`). The flag is
   **transitive through enclosing closures** (`compute_free_vars` ORs it up from
   nested codes): `.map({ start { $c = $c + 1 } })` reaches the outer `my Int $c`
   only via the map closure's capture, and the boxing decision runs at the outer
   creation site. Pin: `t/thread-shared-scalar-visibility.t` test 5.
5. **`cas` swaps through a boxed scalar's cell** (`scalar_cell_target`) instead of
   writing a plain value over the cell in env and stranding the owning slot.
   `builtin_atomic_add_var` and the array/hash forms still resolve by name.

`@`/`%` aggregates always keep the name lane (their `__mutsu_atomic_*` CAS copies
key off those entries), as do `__mutsu_*` internal keys (root-resolved,
ADR-0010) and `state` cells.

## Load-bearing facts (do not re-litigate)

- **`thread_redeclared_vars` is load-bearing.** PR #4654's PLAN step 4 planned to
  delete it; ADR-0010 (first draft) tried and regressed
  `roast/integration/advent2013-day14.t` deterministically — the mask also
  separates same-named bindings in *different routines* (a worker's
  `if G.parse($_) -> $parsed {}` vs the parent's `my $parsed = Channel.new`).
  This change *narrows* the lane and *extends* the mask; it deletes nothing.
- **A cell must supersede the mask.** `box_captured_lexicals` removes the boxed
  name from `thread_redeclared_vars` when it installs the cell in the store;
  leaving the mask in place lets the stale plain snapshot be written back over
  the cell after the next await.
- **Boxing `Instance`-holding scalars was tried and reverted** (would fix
  `$obj = Foo.new` rebind visibility): it breaks `my Lock $l .= new` when two
  sibling blocks declare the same name, because `resolve_capture_slot`'s
  `rposition` fallback resolves the capture to the LAST same-named slot. That
  duplicate-slot hazard belongs to the lexical-scope slot campaign. The gap is
  the `todo` in `t/thread-shared-scalar-visibility.t`.
- **PR #4654's day14 CI failure does not reproduce on the lineage store.** It was
  an interaction with the pre-ADR-0010 process-global flat map.

## Validation

- Pins: `t/recursive-start-await.t`, `t/thread-shared-scalar-visibility.t`,
  `t/shared-var-nil-redeclared-mask.t`, `t/shared-store-lineage-scope.t`,
  `roast/S17-promise/nonblocking-await.t`, `roast/integration/advent2013-day14.t`,
  `roast/S17-lowlevel/cas.t`, `roast/S12-construction/roles-6e.t`.
- The 227 concurrency-related `t/` files, full `make test`, and a local full
  `make roast` (release), plus CI.
