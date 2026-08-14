# Two inline `start` blocks write their stale captured env over a variable declared after them

Extracted from PLAN.md §6 (2026-08-02); found 2026-07-23 while testing WASM concurrency, and
re-verified on `main`2026-08-02 — it is not a WASM artefact. **Still reproduces 2026-08-14**, after
the two prerequisite campaigns this ticket originally deferred to both completed
(`news/2026-08/needs-env-sync-blanket-removal-complete.md`, ADR-0018 slot-addressed lexical capture;
`news/2026-08/closure-env-capture-cost-resolved.md`) — this is a residual case those campaigns did not
cover, not something they were expected to fix incidentally.

## Repro

```raku
my $p = Promise.allof(start { 1 }, start { 2 }); await $p; say $p.WHAT;
# mutsu: Nil        raku: (Promise)
```

Narrowed 2026-08-14: the `await` target does not matter — even an unrelated `await Promise.in(0.05)`
(never touching `$p` at all) triggers the same clobber:

```raku
my $p = Promise.allof(start { 1 }, start { 2 });
say $p.WHAT;                    # (Promise) -- correct right after assignment
await Promise.in(0.05);         # unrelated await
say $p.WHAT;                    # Nil -- clobbered anyway
```

This proves the corruption happens BEFORE any `await` runs at all: `shared_vars["p"]` is already wrong
by the time the first `await` (of anything) triggers `sync_shared_vars_to_env`'s pull-back into `env`.
The bug is therefore in the `start`-block spawn/exit path, not in `await`/`allof`/`anyof` themselves —
confirming the ticket's original "not a `Promise` bug" framing, now narrowed further.

## What's now known (2026-08-14 investigation, not yet a fix)

`cargo run -- --dump-bytecode -e 'my $p = Promise.allof(start { 1 }, start { 2 }); await $p;'` shows
the mainline:

```
0: SetVarDynamic { name_idx: 0, dynamic: false }      # "p" -- pre-declares/hoists the name
1: GetBareWord(1)  ...
4: CallFunc { name_idx: 2, arity: 1, ... }             # start { 1 }
...
8: CallFunc { name_idx: 2, arity: 1, ... }             # start { 2 }
9: ContainerizePair
10: CallMethodMut { ... }                              # .allof(...)
11: SetLocalDecl { slot: 0, explicit_init: true }       # $p = <result>  -- the REAL assignment
...
locals: ["p"]
```

So `$p`'s name is hoisted into `env` (instruction 0, before either `start` block spawns) well before
its real value is assigned (instruction 11, after both `start` blocks have already spawned). Both
`start` blocks therefore capture/seed from a parent `env` that already contains `"p"` — with whatever
`SetVarDynamic`'s hoist leaves it as (need to confirm the exact value; plausibly `Nil` or an
uninitialized placeholder).

`spawn_callable_promise` (`src/runtime/builtins_system.rs`) calls
`self.clone_for_thread_for_block(&block)` — which walks the FULL parent `env` and seeds `shared_vars`
from it (`clone_for_thread_excluding` in `src/runtime/runtime_thread.rs`) — SYNCHRONOUSLY, on the
parent thread, before `worker_pool::submit` queues the block's actual (async) execution. So both
`start` blocks' seeding happens before instruction 11's real assignment, in program order, and that
assignment's own `set_shared_var_sym` call (`src/runtime/runtime_shared_vars.rs`) should then correctly
overwrite `shared_vars["p"]` with the real `Promise` (its `if self.shared_vars.contains_key(key)` guard
passes because the seeding already created the entry). This part of the trace does NOT explain the bug
on its own.

**The likely actual culprit, not yet confirmed with a debugger**: `sync_env_from_locals`
(`src/vm/vm_env_helpers.rs`, around line 1387) — called at frame teardown (`run_inner`) — is
documented as DELIBERATELY publishing every local back to the cross-thread `shared_vars` store
(unlike its two siblings `sync_env_from_locals_declared` / `sync_regex_interpolation_env_from_locals`,
which both wrap themselves in `suppress_shared_publish = true`). Its own comment explains why: roast
`S17-channel/stress.t`'s `sub bogosort_concurrent(@list)` needs its `@list` parameter visible to a
`start` block spawned from inside it, and narrowing this publish previously broke that. If a `start {
1 }` bare-block argument's compiled body shares (or the interpreter mistakenly still points
`current_code`/`locals` at) the ENCLOSING mainline's `CompiledCode` — which does have `"p"` in its
`locals` table — then when that child thread's execution of the trivial block finishes and tears down,
`sync_env_from_locals` would iterate the mainline's locals (including slot 0, "p"), find whatever stale
value the CHILD's own clone holds for it (frozen at spawn/seed time, never touched by `{ 1 }`), and
republish it to `shared_vars["p"]` — asynchronously, so it can race with (and land after) the parent's
own correct instruction-11 write. This was NOT confirmed with a debugger this round (ran out of safe
investigation time) — the next session should:

1. Break on `sync_env_from_locals` (or instrument it) and check, for this exact repro, whether it is
   ever called from the CHILD thread executing `{ 1 }` / `{ 2 }`, and if so, what `code` (whose
   `.locals`) it is walking, and whether `"p"` is among them with a `Nil`/stale value.
2. If confirmed: the safe fix is almost certainly narrowing `sync_env_from_locals` to publish only
   locals the CURRENT FRAME actually initialized/wrote during its own execution — not blindly all of
   `code.locals` — while preserving the `S17-channel/stress.t` case (a sub's own parameter, which IS
   written by that frame, at call-binding time). This needs a real per-frame "which slots did I
   actually write" signal (not present today per this trace), not a name-based exclusion list.
3. Verify against `t/lock.t`, `roast/S17-channel/stress.t`, and this ticket's own repro (as a new
   pinned `t/*.t`) before landing — this function's history (see its own comments) shows narrowing it
   is exactly the kind of change that has silently regressed cross-thread visibility before.

## Second repro, not re-verified this round

```raku
my @p = (start { 1 }, start { 2 }); my $q = Promise.anyof(@p); await $q;
# $q survives
```

(Binding the promises to `@p` first, rather than passing the `start` blocks inline as call arguments,
avoids the bug — consistent with the theory above: `@p`'s own declaration point is BEFORE the `start`
blocks spawn in that shape too, so this needs a closer look at why it differs. Not investigated this
round.)
