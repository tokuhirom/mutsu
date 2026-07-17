# Recursion through a `start` block returns a silently wrong answer

Status: **investigation + a first attempt that is NOT mergeable.** The bug is real
and root-caused; the fix on branch `fix-recursive-start-shared-vars` (PR #4654)
corrects the reported cases but regresses `roast/S17-promise/nonblocking-await.t`,
and the approach it takes has been shown to be structurally fragile (see
"Why the seeding-exclusion approach keeps leaking" below). This document exists so
the next session can restart from the better entry point (Track 2) instead of
re-deriving all of this.

## The bug

```raku
sub f($n) { start { $n <= 0 ?? "b" !! await(f($n - 1)) ~ "|$n" } }
say await f(3);   # b|1|2|3   correct
say await f(3);   # b|3|3|3   WRONG, no error   (raku: b|1|2|3)
say await f(2);   # b|2|2     WRONG             (raku: b|1|2)

sub k($n) { $n <= 0 ?? "b" !! (await start { k($n - 1) }) ~ "|$n" }
say k(3);         # b|1|1|1   WRONG ON THE FIRST CALL (raku: b|1|2|3)
```

Routing through a `my` copy is worse (`b|0|0|0`), and a two-branch `fib`
(`await(fib($n-2)) + await(fib($n-1))` inside `start`) hangs deterministically.
Plain recursion (no `start`) and non-recursive `start` are both correct.

Reproducer: `docs/probes/pool-recursive-start.raku` (all 9 lines must match raku).
Controls that must keep passing: `docs/probes/pool-non-repro-controls.raku`.

## Root cause

`clone_for_thread` (`src/runtime/runtime_thread.rs`) seeds **every** lexical into
`shared_vars`, a flat map keyed by the **bare name**
(`HashMap<String, Value>`). It cannot represent **two concurrently-live bindings of
the same name**, which is exactly what a recursive frame chain is.

The corruption is produced by the write-back that feeds the flat map back into slots:

- **write**: every plain lexical store runs `flush_local_to_env`
  (`vm_env_helpers.rs`) → `set_env_plain_lexical` → `set_shared_var_sym`
  (`runtime_shared_vars.rs`), so each recursive frame's `$n` bind overwrites
  `shared_vars["n"]` and marks it dirty.
- **read-back**: `await` calls `sync_shared_vars_to_env`
  (`runtime_shared_vars.rs`), which pulls every dirty key into `env` **and** queues
  it in `pending_caller_var_writeback`. Its drain,
  `apply_pending_caller_var_writeback` (`vm_env_helpers.rs`), is retain-on-miss and
  deliberately **walks UP the frame chain** until some frame owns a slot named
  `"n"`. So the innermost write is force-fed into every outer frame's slot.
  `k`/`g` are the `insert`-overwrite direction; `f`'s 2nd call is the
  `or_insert_with` direction (the stale value from run 1 survives).

Crucially, `start` already compiles its block as escaping
(`compiler/expr_call.rs`, `escaping_args = name == "start"`), so
`box_captured_lexicals` (`vm/vm_register_ops.rs`) **already** gives each frame's
`$n` a correct per-binding home — a shared `ContainerRef` cell when it is mutated,
a frozen value when it is read-only. **The flat name-keyed map is a second, lossy
mechanism running in parallel with a working one, and it overwrites the working
one's correct answer.** This is the "1 operation = 1 implementation" violation.

## Track 1 (attempted, on the branch): stop seeding a block's own captures

Approach: in `clone_for_thread_for_block` (a new entry point used only by
`spawn_callable_promise`), skip seeding the scalars the block captures itself
(`cc.free_var_syms`), so the working cell mechanism is left to own them; keep
`@`/`%` (their `__mutsu_atomic_*` CAS copies key off these entries).

This fixes the reproducer and passes `make test` (1786 files / 17467 tests). But it
kept leaking regressions as the roast surface widened, because the flat scalar lane
turned out to be **load-bearing for four distinct jobs**, not one:

| Surfaced by | The job the name lane was secretly doing |
|---|---|
| the reproducer | carry a recursive frame's `$n` (the bug — this one it did *wrong*) |
| `roast/S12-construction/roles-6e.t`, `t/destroy-cross-thread-writeback-coherence.t` | carry a write from a **class/role method** (`submethod DESTROY { $a++ }`) that the capture analysis cannot see — the method is installed by `RegisterClass`/`RegisterRole` with no closure-creation op |
| `roast/integration/advent2013-day14.t` | keep a parent `my $parsed = Channel.new` distinct from a **worker's** same-named `if G.parse($_) -> $parsed {...}` — same-name binding in a *different routine*, which is what `thread_redeclared_vars` guards |
| `t/concurrency-threading.t` | keep parent and worker on **one object** for a captured scalar holding a non-boxable value (`Channel`, `Lock`, a `List`, …) — `box_captured_lexicals` declines to box those, so there is no cell to fall back to |
| `roast/S17-promise/nonblocking-await.t` | **still failing** — an env-refresh the lane performs; adding a single `say` before `await $started` makes all 28 subtests pass (the `say` forces an env flush), so the exclusion leaves a coherence hole this doc has not yet root-caused |

Each was patched in turn (see the commit and PLAN §6), but the pattern — fix one,
the next appears — is the signal that Track 1 is patching symptoms of removing a
mechanism whose full set of responsibilities was never enumerated up front.

### Load-bearing facts established while chasing Track 1 (do NOT re-litigate)

- **Typed-scalar boxing must be thread-escape-only.** Boxing a type/`where`-constrained
  scalar into a cell is needed so a worker's write is visible
  (`my Int $c = 0; await start { $c = 5 }`), and it does NOT weaken the constraint
  (the check runs at the assignment op, by name, in `var_type_constraints` — cloned
  into the child — *before* the write-through). But it must NOT happen for a
  **same-frame** closure, because `cas` resolves its target by name and is not
  cell-aware: `roast/S17-lowlevel/cas.t` does `cas` on a `my LittleNodey $head`
  captured by a `throws-like { ... }` block. Gated by the new
  `CompiledCode::thread_escaping` flag (set for `start` args and the `.start`
  method).
- **`thread_redeclared_vars` is load-bearing; deleting it is wrong.** An earlier
  step of Track 1 deleted it (PLAN's step 4 assumed it dies once seeding stops);
  `advent2013-day14.t` proves it does not, because that mask also separates
  same-named bindings in *different routines*, not just a block's own captures.
  The earlier claim that this change "subsumes #4650" is **retracted** — #4650's
  pin passed only because it does not cover the day14 shape.
- **A cell must supersede the mask.** With the mask restored, `my $s2 = 0` declared
  *after* a prior thread then written inside `await start { $s2 += … }` left the
  parent `Nil`: `box_captured_lexicals` replaces the stale plain snapshot in the
  store with the cell (`set_shared_var(name, cell)`), and the mask was blocking
  that replacement, so the stale value was written back over the cell at the next
  await. Fix: `thread_redeclared_vars.remove(name)` when boxing.
- **The seeding-exclusion allow-list, not a deny-list.** Only genuinely plain
  scalars (`Int`/`Num`/`Str`/`Bool`/`Rat`/…/`ContainerRef`) are owned per binding
  by the closure machinery. Everything else (`Channel`, `Promise`, `Lock`, an
  `Array`/`Hash`/`List`, a `Sub`, a type object) is a shape `box_captured_lexicals`
  declines to box, so excluding it from the lane hangs
  `my $c = Channel.new; start { $c.send(42) }; $c.receive`
  (`t/concurrency-threading.t` test 4).
- **`cas` is only half cell-aware.** `builtin_cas_var` now swaps through a boxed
  scalar's `ContainerRef` (`scalar_cell_target`), but `builtin_atomic_add_var` and
  the array/hash forms still resolve by name.

### Rejected while chasing Track 1

- **Boxing `Instance`-holding scalars** (would fix `$obj = Foo.new` rebind
  visibility across a thread): breaks `my Lock $l .= new` (the `.=` counts as a
  capture-mutation) when two sibling blocks declare the same name, because
  `resolve_capture_slot`'s `rposition` name search resolves the capture to the
  LAST same-named slot. That duplicate-slot hazard is the `dup_shadow_possible`
  gate's territory = the lexical-scope slot campaign, not this bug. The resulting
  gap is a `todo` in `t/thread-shared-scalar-visibility.t`.
- **Excluding only *celled* scalars from seeding**: recursion corruption returns,
  because `$n` is read-only → frozen by value → has no cell. `$n` and a
  DESTROY-written `$a` are structurally identical to the compiler (both read-only
  free vars of the block); the only separator is "does a registered method write
  it", which is why that case needs `type_body_written_lexicals`.

## Track 2 (recommended entry point for the next session): fix the write-back, not the seeding

The corruption's actual mechanism is **not** the seeding — it is
`apply_pending_caller_var_writeback` walking up the frame chain and force-feeding
the innermost value into ancestor slots (see "Root cause" above). Track 1 tried to
remove the *input* to that mechanism (stop seeding), which is why it kept colliding
with the lane's other users. Track 2 leaves seeding untouched and constrains the
write-back so it can only reach the frame that actually owns the binding:

- The pending caller-var write-back should target a specific frame/slot identity,
  not "the nearest ancestor frame that happens to own a slot named `n`". A recursive
  chain has many such frames; the retain-on-miss upward walk is what smears the
  value across all of them.
- This leaves the flat scalar lane intact for its four other jobs (class/role
  method writes, cross-routine same-name separation, non-boxable captured objects,
  the env-refresh that `nonblocking-await.t` depends on), so it should not
  reintroduce those regressions.

This was not attempted this session — Track 1 was entered from the seeding side by
mistake. Start here.

## What is on the branch right now

`fix-recursive-start-shared-vars` (PR #4654, auto-merge on, CI RED). The pushed
commit is stale; the working tree has the Track 1 fix plus every patch above, and
still fails `roast/S17-promise/nonblocking-await.t`. New tests added:
`t/recursive-start-await.t` (the reproducer, promoted from `docs/probes/`) and
`t/thread-shared-scalar-visibility.t` (the shapes `box_captured_lexicals` declines
to box; 2 documented `todo`s). Decide per Track 2 whether to keep, rework, or
abandon the branch's code; the tests and this document are worth keeping regardless.
