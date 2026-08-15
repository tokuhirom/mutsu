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

## CONFIRMED root cause (2026-08-15, via `rust-gdb`) -- prior session's hypothesis was WRONG

The 2026-08-14 session's hypothesis (a CHILD thread executing `{ 1 }`/`{ 2 }` walks the mainline's
`CompiledCode.locals` at its own `run_inner` teardown and republishes a stale `"p"`) was tested with
`rust-gdb -batch` and **refuted**: a Python breakpoint on `sync_env_from_locals`
(`src/vm/vm_env_helpers.rs:1388`) logging thread id + backtrace on every hit shows **zero hits from
either worker thread** for this repro. Every hit is on the single VM execution thread (main program
thread; see "the VM runs on a spawned thread" in this file's Debugging guidelines).

The REAL mechanism, confirmed end-to-end with three separate breakpoints:

1. **Where the bad value gets planted.** `dispatch_func_call_inner`'s `if name == "start" {
   self.sync_env_from_locals(code); }` special case (`src/vm/vm_call_func_ops.rs:1503`, duplicated in
   `exec_call_func_op` at `:1118`) runs SYNCHRONOUSLY on the main thread, once per `start { ... }` call,
   BEFORE the block is spawned. For `my $p = Promise.allof(start { 1 }, start { 2 })`, both `start`
   calls are `.allof`'s arguments, evaluated before `$p`'s own `SetLocalDecl` (instr 11) ever runs — so
   at both sync points `self.locals[0]` ("p") still holds its `SetVarDynamic`-hoisted pre-assignment
   placeholder (Nil). `sync_env_from_locals` blindly publishes EVERY local in `code.locals` (no gate at
   all), so it writes `shared_vars["p"] = Nil` via `set_env_with_main_alias` -> `set_shared_var_sym`,
   which also calls `mark_shared_var_dirty("p")` (`src/runtime/runtime_shared_vars.rs:534-540`).
   Confirmed via gdb: breaking on `vm_call_func_ops.rs:1503` shows this firing (repeatedly, once per
   `start` call) purely on the main thread, always before `SetLocalDecl`.

2. **Why the real assignment never overwrites it.** `$p`'s real `SetLocalDecl` (instr 11) writes
   `self.locals[0] = Promise` correctly, but the ordinary per-store write path
   (`exec_set_local_op_inner`, `src/vm/vm_var_assign_set_local.rs`) computes `skip_env_write =
   !code.needs_env_sync[idx] && ...` (line 1826) and, when true, **does not** call
   `set_env_plain_lexical`/`set_shared_var_sym` for this store (`src/vm/vm_var_assign_set_local.rs:1835-1844`,
   the "(B) per-store env-write" ADR-0018-era optimization: a slot the compiler proved has no
   closure/reflective/named-sub reader by name is its own single source of truth, so env/shared_vars
   mirroring is skipped). Confirmed via gdb (breakpoint at `vm_var_assign_set_local.rs:1831` printing
   `skip_env_write`): **`skip_env_write=true`** for slot 0 ("p") at this exact assignment, because `$p`
   is never referenced by name inside the `start { 1 }`/`start { 2 }` closures (they don't touch it at
   all) or anywhere else — `code.needs_env_sync[0] == false`. So `shared_vars["p"]` is left at the stale
   `Nil` from step 1, still marked dirty. `self.locals[0]` itself IS correct, and `GetLocal(0)` (a direct
   read of `$p`, e.g. `say $p.WHAT` right after the assignment) reads it correctly — which is why the bug
   is invisible until the next step.

3. **How the stale value clobbers the correct one.** `await` (of ANYTHING, confirmed by the narrowed
   repro below) calls `builtin_await` -> `sync_shared_vars_to_env()`
   (`src/runtime/runtime_shared_vars.rs:576`), which pulls every DIRTY `shared_vars` key back into `env`
   (line 696: `self.env.insert(key, val)`) and queues the name into `pending_caller_var_writeback`. Since
   `"p"` was marked dirty in step 1 with the stale Nil, this overwrites `env["p"]` with Nil. Then
   `apply_pending_caller_var_writeback` (`src/vm/vm_env_helpers.rs:1617`, drained right after the call
   op returns, `exec_call_func_op`) finds `"p"`'s slot in the mainline's own `code.locals` and does
   `self.locals[slot] = env["p"]` (line 1628) -- **directly overwriting the correct `self.locals[0]`
   (the real `Promise`) with the stale `Nil`.** Confirmed via gdb: a breakpoint on
   `vm_env_helpers.rs:1628` conditioned on `source == "p"` fires immediately after `await`'s
   `sync_shared_vars_to_env` calls, with a backtrace through `builtin_await` -> `exec_call_func_op`.
   `say $p.WHAT` afterward now reads the clobbered slot and prints `Nil`.

This chain is now confirmed for both repros in this ticket (the `await $p` one and the narrowed
`await Promise.in(0.05)` one), fully explaining why the `await` target doesn't matter: step 3 fires for
ANY `await` that reaches `sync_shared_vars_to_env`, regardless of what it's awaiting, because the
corruption (steps 1-2) already happened synchronously before any `await` ran.

### Fix attempt 2026-08-15 -- tried, REVERTED, confirmed unsafe

Given step 1's `sync_env_from_locals` has no gate at all while step 2's ordinary write path already has
exactly the right gate (`code.needs_env_sync[idx]` -- a compiler-computed, per-slot signal, not a
name-based heuristic), the natural fix is to make step 1 respect the same gate: add
`sync_env_from_locals_needed` (only at the two "before start spawn" call sites,
`vm_call_func_ops.rs:1118` and `:1503` -- NOT touching `sync_env_from_locals`'s other 6 callers, e.g.
frame teardown, which have a different justification/history), skipping a slot when
`!code.needs_env_sync[idx]`.

This is semantically well-motivated: `compute_needs_env_sync`'s nested-closure free-var scan
(`src/opcode.rs:4607-4625`) already walks `closure_compiled_codes` (which includes a `start { ... }`
block's `MakeAnonSub` body) and sets `needs_env_sync[slot] = true` for any local a nested closure reads
BY NAME via a `GetOuterVar`-recorded free-var -- exactly the mechanism the `S17-channel/stress.t`
`bogosort_concurrent(@list)` case needs (`@list` IS read by name inside its `start` block, so its
`needs_env_sync` is `true` regardless of this change). Verified: `t/lock.t` (3x), `roast/S17-channel/stress.t`
(3x, ~11-15s each), and both of this ticket's repros (5x each) all passed cleanly with the fix in.

**But it is UNSAFE and was reverted.** Running the full `t/` suite (`make test`, 3169 files) surfaced a
real regression: `t/promise-combinator.t` subtest "allof.result syncs cas updates from worker promises"
started failing deterministically (`~$seen` came back `' 1'` instead of `'1'`):

```raku
my $seen = [];
Promise.allof(start { cas $seen, -> @current { flat @current, 1 } }).result;
is ~$seen, '1', ...;
```

Confirmed via gdb (breakpoint in `sync_env_from_locals_needed` printing `code.needs_env_sync`) that
**`needs_env_sync[slot for "seen"] == false`** even though the `start` block's `cas $seen, ...` body
genuinely depends on seeing/mutating `$seen` cross-thread. `cas`'s target argument is a **rw-arg sink**,
not an ordinary `GetOuterVar` free-var read or one of the `op_container_mutate_const_idx`-recognized
in-place-mutation forms (`:delete`, `$h<k>=v`, `$a[i]++`) that `compute_needs_env_sync`'s nested-closure
scan (`src/opcode.rs:4607-4650`) already special-cases -- so it is invisible to `needs_env_sync`
entirely. This is not a novel gap: this repo's own `CLAUDE.md` ("What 'gain' and 'risk' actually mean")
already documents that "mutsu's compile-time mutation analysis is incomplete (it does not see writes
from separately-registered role/class methods, nor **rw-arg sinks like `cas`**)" as a known reason a
by-value/incomplete-static-analysis shortcut is the RISKY choice, not the safe one. Gating
`sync_env_from_locals`'s pre-spawn publish on `needs_env_sync` is exactly that shortcut, and the `cas`
case is exactly the documented gap it falls into.

Verified this is caused by the fix, not pre-existing: reverted the change (`git stash` / rebuild /
`prove -e target/debug/mutsu t/promise-combinator.t`) and confirmed the unmodified baseline passes
cleanly; restored the fix (`git stash pop`) and confirmed it fails again, deterministically (not a
one-off/flaky result). **The fix was then fully reverted** (`git checkout -- src/vm/vm_call_func_ops.rs
src/vm/vm_env_helpers.rs`) and the new pinned test file removed -- the working tree is back to
unmodified `main`. No PR was opened for a code change.

### Next steps for whoever picks this up

`needs_env_sync` is the right KIND of signal (compiler-computed, per-slot, already trusted by the write
path) but is currently INCOMPLETE for this purpose: it needs to also cover rw-arg-sink reads inside a
nested closure (starting with `cas`, but audit for other rw-arg builtins/forms with the same shape --
anything that takes a variable "by reference" rather than by value inside a `start`/closure body).
Concretely: extend `compute_needs_env_sync`'s nested-closure scan (`src/opcode.rs:4607-4650`, alongside
the existing `free_var_syms` loop and the `op_container_mutate_const_idx` loop) with a THIRD pass that
recognizes `cas`'s (and any sibling rw-arg builtin's) target-variable argument the same way the
container-mutate pass recognizes `:delete`/`$h<k>=v`/`$a[i]++` -- i.e. find how `cas $var, ...` is
compiled (what op/arg-source records `$var` as an rw target) and fold that name's slot into
`needs_env_sync` too. Once that is done and independently verified NOT to reintroduce this ticket's
original bug, re-apply the `sync_env_from_locals_needed` gate at the two "before start spawn" call
sites and re-run the full verification battery: `t/lock.t`, `roast/S17-channel/stress.t`,
`t/promise-combinator.t` (all 3-5x, concurrency tests), this ticket's two repros as a new pinned
`t/*.t`, plus the full `t/` suite and a broad `roast/S17-*` sweep before opening a PR.

Do NOT re-attempt the gate without first fixing the `needs_env_sync` completeness gap above -- it WILL
reintroduce the `t/promise-combinator.t` regression, confirmed twice in this round (with and without
the fix, via `git stash`/`git stash pop`).

## What's now known (2026-08-14 investigation, superseded above -- kept for history)

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
