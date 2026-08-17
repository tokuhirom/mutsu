# Two inline `start` blocks write their stale captured env over a variable declared after them

**Reclassified from `todo/tickets/` to `todo/deep/` on 2026-08-17** (per `todo/README.md`'s
tickets-vs-deep split): three separate sessions (2026-08-15, 2026-08-16, 2026-08-17) have each
made real progress and each hit a genuine, hard blocker before landing a fix — most recently a
pre-existing, unresolved interaction between `needs_env_sync` and the atomic-scalar-cell RMW
subsystem ("Gap 4" below) that needs its own root-cause investigation, not a quick patch. This is
no longer a "pick it up and finish in a session" item.

**Also blocks an OTF-compilation gate exclusion.** `todo/tickets/otf-compilation-gate-leftovers.md`
(retired 2026-08-17, folded in here) tracked a *different* symptom of the same underlying
capture-cell gap: `expr_needs_interpreter` (`src/vm/vm_call_func_ops.rs`, ~line 1996) forces any
`start` block onto the slow interpreter fallback rather than the OTF-compiled path, because a
recursive sub's `start` closure capturing a parameter gets clobbered by the recursive call's
re-bind (regression pin: `t/start-block-return-value.t` test 3; proof of infeasibility and history
in `news/2026-07.md`). The real fix for both is per-call capture cells / a sound cross-thread env
signal — whoever resolves the bug below should re-check whether `expr_needs_interpreter`'s
`"start"` exclusion can also be lifted at the same time.

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

### Fix attempt 2026-08-17 -- the `needs_env_sync` completeness gap WAS closed, but surfaced a DEEPER,
### unrelated pre-existing bug in the atomic-scalar-cell subsystem; REVERTED again

This round followed the previous session's "next steps" exactly and made real progress -- two
completeness gaps in `needs_env_sync` were found and durably fixed -- but the gate itself still had to
be reverted because it exposes a third, much deeper pre-existing bug that is out of this ticket's
scope. Full trace below so the next session does not repeat the same three-gap discovery from scratch.

**Gap 1 (as predicted): `cas`'s free-var completeness.** Confirmed via the compiler
(`src/compiler/expr_call.rs`): `cas $var, ...` compiles to `LoadConst(<var name string>)` +
`CallFunc("__mutsu_cas_var", ...)` -- the target reaches the callee as a **string constant**, never an
ordinary `GetOuterVar`/`GetGlobal` read, so `compute_free_vars`'s op scan can never see it. `cas` is
also deliberately excluded from `atomic_target_syms` (`note_atomic_env_sync_target(&var_name, false)` at
its direct-`cas` call site, `expr_call.rs` ~line 781) to avoid the cell-promotion fold that the sibling
`atomic-*` builtins (`atomic-fetch`, `⚛$x`, `$x⚛++`, ...) DO opt into via `counts_as_write: true`.

**Fix 1 (worked, in isolation):** added a new `CompiledCode::rw_arg_env_sync_syms:
FxHashSet<Symbol>` field, populated unconditionally (regardless of `counts_as_write`) in
`note_atomic_env_sync_target` (`src/compiler/expr_helpers.rs`). Consumed in `compute_free_vars`
(`src/opcode.rs`, right after the existing `atomic_target_syms` fold) by folding it into `free`
**without** touching `free_writes`/`self_mutated` -- i.e. it gives `cas`'s target READ status (so it
participates in `free_var_syms`, and through it, `needs_env_sync`) without giving it WRITE status (so
it does NOT trigger cell promotion, preserving the exclusion `cas` needs). This alone fixed
`t/promise-combinator.t`'s regressed subtest.

**Gap 2 (new this round): multi-level closure nesting.** A first version of Fix 1 walked
`nested.rw_arg_env_sync_syms` directly in `compute_needs_env_sync`'s nested-closure scan (one level up
only, mirroring the existing `op_container_mutate_const_idx` pass). That is NOT enough: roast
`S17-scheduler/every.t` (`$*SCHEDULER.cue({ cas $count, {.succ} }, every => Inf)` wrapped in
`lives-ok { $c1 = $*SCHEDULER.cue(...) }`) regressed tests 22/24 -- `cas $count` sits behind an
INTERMEDIATE closure (`lives-ok`'s block argument), so a one-level fold in the OUTER frame's
`compute_needs_env_sync` never reaches it. **Fix 2:** since `free_var_syms` is ALREADY folded
transitively through every nesting level by `compute_free_vars`'s own "fold a nested closure's
`free_var_syms` into mine" passes, folding `rw_arg_env_sync_syms` into `free` INSIDE
`compute_free_vars` (Fix 1, above) automatically reaches every ancestor frame for free -- no separate
multi-level pass needed. The redundant one-level `compute_needs_env_sync` pass was removed once this
was confirmed (`roast/S17-scheduler/every.t` tests 22/24 both green).

**Gap 3 (env pre-init placeholder, explored and reverted): a DIFFERENT test,
`t/shared-var-nil-redeclared-mask.t`'s "bind stays Nil across a spawn between declaration and read",
regressed too** (`my $depends := nil-returner(); await start { 1 }; $depends` -- an UNRELATED `start`,
no `cas` anywhere). Root cause, confirmed via `rust-gdb`: `exec_set_var_dynamic_op`
(`src/vm/vm_var_assign_set_local.rs`) unconditionally seeds `env[name] = Package("Any")` at
declaration time ("Pre-initialize the variable in the env with a default value so that closures
created during the RHS expression can capture it") -- for a Nil-bound scalar with `needs_env_sync ==
false`, the ACTUAL declaration store then skips its own env mirror (the pre-existing "(B) per-store
env-write" gate), so this placeholder is NEVER corrected and stays `Any` in `env` forever. On baseline,
this was invisible because `sync_env_from_locals`'s BLIND publish overwrote it with the correct value
on every `start` spawn (the exact behavior this ticket's whole gate removes). Two things were tried
and rejected for this gap:
  - Skipping the placeholder seed (write-side) AND the `undeclared_variable` check's env-presence
    requirement (read-side, `exec_get_local_op`) both gated on `needs_env_sync[idx]`: this "fixed" the
    Nil-mask test but broke `t/undeclared-symbol-exception-class.t`, `t/block-lexical-scope.t`,
    `t/package-block-lexical-capture.t`, `t/regex-counted-adverbs.t`,
    `t/regex-p5-smartmatch-regressions.t`, `t/throws-like-outer-var-writeback.t` -- because
    `needs_env_sync == false` does NOT distinguish "declared, deliberately slot-only" from "genuinely
    never declared at runtime" (e.g. `{ my $x = 1 }; $x` reading a name whose enclosing block's `my`
    never ran): both cases end with a Nil slot and no env entry, but only one should throw
    `X::Undeclared`. **REVERTED** (both files back to original).
  - **The fix that actually worked for gap 3, isolated:** keep the declaration-time seed AND the
    `undeclared_variable` check fully unconditional (untouched, matching baseline exactly) -- instead
    made `sync_env_from_locals_needed` (the ticket's own gated function) write `env` UNCONDITIONALLY
    for every slot (matching `sync_env_from_locals`'s original behavior byte-for-byte on that half) but
    gate ONLY the cross-thread `shared_vars` publish, per slot, via the existing
    `suppress_shared_publish` flag (toggled per-iteration around the `set_env_with_main_alias` call).
    Rationale: keeping `env` always coherent with `self.locals[i]` at every spawn means the
    declaration-time placeholder is ALWAYS corrected before any spawn (env is the accurate live
    snapshot, never stale), so `clone_for_thread_excluding`'s own independent env-walk (a SEPARATE
    mechanism from this ticket's gate, with no per-slot `needs_env_sync` visibility of its own) always
    seeds `shared_vars` correctly too -- without touching declaration/read-side machinery at all. This
    passed the FULL `t/` suite (3192 files) cleanly, including all six files gap-3's first attempt
    broke.

**Gap 4 (the actual blocker this round, NOT resolved): `apply_pending_caller_var_writeback`'s
`needs_env_sync` gate breaks the atomic-scalar-cell subsystem.** During the earlier 2026-08-15 round, a
gate was ALSO added to `apply_pending_caller_var_writeback` (`src/vm/vm_env_helpers.rs`) -- skip
pulling a dirty cross-thread value into `self.locals[slot]` when `needs_env_sync[slot]` is false, on
the theory that such a slot never legitimately publishes by name so a name match must belong to an
unrelated frame. With gaps 1-3 above fixed and the full `t/` suite green, a full `roast/S17-*/*.t`
sweep (99 files) surfaced ONE new failure: `roast/S17-scheduler/every.t` tests 22/24 initially (fixed
by gap 2's transitive fold), and after that fix, a full `t/` re-run surfaced
`t/cross-thread-shared-var-writeback-coherence.t` subtest 3, **"awaited cas scalar increment visible in
caller slot"**:
```raku
my $n = 0;
Promise.allof(start { cas $n, -> $v { $v + 1 } }).result;
is $n, 1, ...;   # got 0
```
Minimal repro (`tmp/repro16.raku` in this session, not preserved -- recreate from this snippet):
this test ONLY fails when preceded by ANOTHER block using `cas` on a DIFFERENT array (e.g.
`{ my $seen = []; Promise.allof(start { cas $seen, -> @c { flat @c, 1 } }).result; }` run first in the
same process) -- standalone, `$n` reconciles correctly. `cas $n, -> $v { $v + 1 }` matches the
compiler's DELTA-LAMBDA rewrite (`expr_call.rs` ~line 744: a 2-arg `cas` whose lambda body is a single
`$v + delta` expression rewrites to `__mutsu_atomic_add_var` directly, `note_atomic_env_sync_target(&
var_name, true)` -- `counts_as_write: true`, UNLIKE the general `cas` path). `builtin_atomic_add_var`
(`src/runtime/builtins_atomic.rs`) has a fast path, `atomic_scalar_cell`
(`src/runtime/builtins_atomic_shared.rs`): if a **shared cell** already exists for the name, it RMWs
the cell directly and returns -- no `shared_vars`/dirty-key publish at all for that update (the cell
IS the shared state). `rust-gdb` confirmed `atomic_scalar_cell` DOES find `Some(cell)` for `$n` in the
failing run. Bisection (temporarily neutralizing each of the four changes independently, rebuilding
each time, all via `git checkout -- <file>` / `git apply -p0 <saved-diff>` -- **NOT `git stash`**, per
this repo's documented stash-sharing trap) showed:
  - Disabling ONLY `apply_pending_caller_var_writeback`'s gate (keeping gaps 1-3's fixes fully active)
    made the repro PASS (`1\n1`).
  - But then re-testing with gap 2's transitive `free_var_syms` fold ALSO active (it was transiently
    disabled during an earlier bisection step) made it FAIL AGAIN even with the writeback gate fully
    removed -- i.e. the transitive fold itself, independent of the writeback gate, ALSO changes
    something that breaks this atomic-cell case. `rust-gdb` on a from-scratch (unmodified `opcode.rs`)
    build confirmed `needs_env_sync["n"]` is **false** even with baseline's own PRE-EXISTING
    `atomic_target_syms` fold (which predates this ticket entirely) -- so gap 1's `counts_as_write:
    true` write-fold, which SHOULD have already propagated `$n` to `free_var_syms` on its own, does
    NOT actually reach the outer frame in this specific two-block program shape. This was not fully
    root-caused: whether the transitive fold changes cell-promotion/upvalue-materialization decisions
    for `$n` (as opposed to only `needs_env_sync`), and why baseline's own `atomic_target_syms` fold
    does not already make `needs_env_sync["n"]` true here, are both still open questions.

**Conclusion:** the `needs_env_sync` gate on `apply_pending_caller_var_writeback` (added 2026-08-15,
BEFORE gaps 1-3 were even discovered) is unsound as currently designed -- `needs_env_sync` is not a
complete signal for "does this slot need cross-thread reconciliation," because the atomic-scalar-cell
subsystem (`atomic_scalar_cell`, `builtin_atomic_add_var`/`builtin_atomic_fetch_add_var`/etc.) performs
its OWN independent cross-thread visibility mechanism (shared `Gc<Mutex<Value>>` cells, RMW'd in place,
with no per-name `shared_vars`/env publish once a cell exists) that `needs_env_sync` was never designed
to model and does not track. Gating `apply_pending_caller_var_writeback` on it therefore has TWO
INCOMPATIBLE requirements that cannot both be satisfied by one signal: `needs_env_sync == false` must
mean "do not resurrect a foreign frame's stale value" for gap 3's Nil-mask case, AND `needs_env_sync ==
false` must NOT mean "skip the writeback" for an atomic-cell scalar target whose `needs_env_sync` this
session could not make reliably true. **The entire round was reverted** (`git checkout --
src/compiler/expr_helpers.rs src/opcode.rs src/vm/vm_call_func_ops.rs src/vm/vm_env_helpers.rs`; the new
pinned test file `t/start-block-inline-arg-locals-clobber.t` was removed) -- the working tree is back to
unmodified `main`. No PR was opened.

### Next steps for whoever picks this up (revised 2026-08-17)

Gaps 1-3 above are SOLVED designs, safe to re-apply verbatim (re-derive from this write-up; the exact
diffs were not preserved as a patch file, but every piece is described precisely enough to reconstruct):
1. `CompiledCode::rw_arg_env_sync_syms` field + `note_atomic_env_sync_target` populating it
   unconditionally + `compute_free_vars` folding it into `free` only (not `free_writes`) -- gap 1 + 2.
2. `sync_env_from_locals_needed`: unconditional `env` write (matching `sync_env_from_locals` verbatim)
   + per-slot `suppress_shared_publish` toggle gating ONLY the `shared_vars` half on
   `needs_env_sync[i]` -- gap 3. Do NOT gate the env write itself (that reopens gap 3 via the
   undeclared-variable / placeholder-seed entanglement documented above).
3. Do NOT add a `needs_env_sync`-based gate to `apply_pending_caller_var_writeback` at all (revert it
   to its pre-2026-08-15 unconditional form if it is ever reintroduced) until gap 4 has a real fix.

Gap 4 needs a fundamentally different signal, not a smarter `needs_env_sync`. Candidate directions,
not evaluated this round:
- Give `apply_pending_caller_var_writeback` visibility into whether the SOURCE name currently resolves
  to a live atomic cell (`atomic_scalar_cell`/`scalar_cell_target`-style lookup) and, if so, always
  apply the writeback (or better, skip the whole env-diffing path and defer entirely to the cell, since
  a cell-backed scalar's `self.locals[slot]` should probably just BE a `ContainerRef` at the CALLER
  too, making the whole writeback question moot for it) -- investigate why the caller's own slot is NOT
  already a matching `ContainerRef` after `await`/`.result` for this case, which may be the more
  fundamental gap.
- Re-scope `pending_caller_var_writeback`'s gate to something more precise than a per-slot boolean:
  e.g. only skip when the frame doing the drain can positively identify the dirty entry as belonging to
  a DIFFERENT, still-live lineage (the actual concern gap 3 was guarding against), rather than inferring
  it from "this slot doesn't publish by name."
- Get a minimal, gap-4-only repro (two sibling top-level blocks, first uses array `cas`, second uses
  scalar delta-lambda `cas`, no `Test`/`lives-ok` involved -- `tmp/repro16.raku`'s shape in this
  session) under `rust-gdb` from a clean baseline FIRST, before layering gaps 1-3's fixes back on, to
  isolate whether `atomic_target_syms`'s pre-existing (this-ticket-independent) fold already fails to
  reach the outer frame in this two-block shape on pure baseline -- that would mean gap 4 is a
  pre-existing dormant bug in `atomic_target_syms`'s OWN propagation, not something gaps 1-3 introduce,
  which would change the fix location entirely (fix the pre-existing propagation gap first, in
  isolation, verify it does not regress anything on its own, THEN re-layer gaps 1-3 and the
  `apply_pending_caller_var_writeback` gate on top).

As before: do NOT re-attempt the `apply_pending_caller_var_writeback` gate without first resolving gap
4 above -- it WILL reintroduce the `t/cross-thread-shared-var-writeback-coherence.t` regression,
confirmed via `git checkout`/`git apply -p0` A/B bisection (not `git stash`) in this round.

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
