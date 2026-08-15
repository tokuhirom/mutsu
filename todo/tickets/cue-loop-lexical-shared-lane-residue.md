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

**Still reproduces as of 2026-08-15** (re-verified against `main` at
`9f34eef44`, i.e. the tip this ticket's investigation branched from).

## Root cause (precisely identified, 2026-08-15 — this supersedes the
## 2026-08-05 trace's "binding-generation" theory below)

The exact mechanism, confirmed via `rust-gdb` breakpoints and a temporary
`eprintln!`-based instrumented build (both removed before this update):

1. `env` only mirrors a local's current value when something in the SAME
   compiled frame reads it BY NAME — the "(B) per-store env-write gate"
   (`compute_needs_env_sync` in `src/opcode.rs`, consumed by
   `flush_local_to_env` / `exec_set_local_op_inner` in
   `src/vm/vm_env_helpers.rs` and `src/vm/vm_var_assign_set_local.rs`). A
   nested closure normally reads its free variables straight from the
   creating frame's LIVE LOCAL SLOTS (`capture_closure_env`), not via `env`,
   so a captured local's store does NOT force an env flush by default.
2. `cas $a, …` is parsed and compiled to `__mutsu_cas_var("a", …)` — the
   variable name becomes a STRING CONSTANT, not a normal `Expr::Var` read.
   This is invisible to `free_var_syms` (the closure-capture analysis), and
   `compiler/expr_call.rs`'s `note_atomic_env_sync_target` DELIBERATELY
   excludes `cas` from the "write" fold that would otherwise force `a`'s
   slot to be env-synced (`n != "__mutsu_cas_var"` guard) — the comment
   there says this is so `cas`'s cross-thread behavior rides the name-keyed
   `shared_vars` lane rather than earning a per-binding
   `ContainerRef` cell (`t/cross-thread-shared-var-writeback-coherence.t`
   pins that choice).
3. So `env["a"]` is never written by `my $a = 0`'s own declaration (gate #1
   skips it) and is never forced by `cas`'s own compiled form (exclusion
   #2). It is written ONLY as a side effect of a LATER `sync_shared_vars_to_env`
   call (`src/runtime/runtime_shared_vars.rs`) reconciling the FINAL
   accumulated `cas` count from a FINISHED cue tick back into the caller's
   env (confirmed via instrumentation: `sync_shared_vars_to_env writing
   env[a] = 10` after round 1, `= 20` after round 2, etc.).
4. The NEXT round's `my $a = 0` redeclaration resets the LOCAL SLOT to 0 but,
   per #1, does NOT touch `env["a"]` — it stays at the stale value from #3.
5. When round N+1's `$*SCHEDULER.cue(...)` clones the interpreter for its
   worker thread (`clone_for_thread_for_block` →
   `clone_for_thread_excluding` in `src/runtime/runtime_thread.rs`), the
   clone's `env` is a snapshot of `self.env` at that moment (`env:
   self.env.clone()`) — still carrying the stale value from step 3.
6. The FIRST `cas` tick of the new round computes a fresh
   `__mutsu_atomic_value::N` key (confirmed via instrumentation: each round
   gets a genuinely fresh key, `::1`, `::2`, `::3`, …) but its "current
   value" fallback (`atomic_current_value` in `src/runtime/builtins_atomic.rs`,
   `shared.get(value_key).or_else(|| self.env.get(name))`) has no entry for
   the brand-new key yet, so it falls through to `self.env.get("a")` — the
   STALE value from step 5. Every subsequent tick within the round then
   legitimately reads its own now-populated `shared[value_key]`, so the
   round accumulates correctly FROM the stale base, giving the observed
   `10, 20, 30, …` pattern.

**Why `start` (bareword) is not affected**: `vm/vm_call_func_ops.rs`'s
`CallFunc`/`CallFuncNamed` handlers had (until this investigation's now-
reverted attempt) a narrow, function-name-hardcoded
`if name == "start" { self.sync_env_from_locals(code) }` that flushed EVERY
local of the calling frame into `env` right before a bareword `start`
call spawned — an unrelated, older mechanism, NOT part of the
`block_captured_scalars`/cell-boxing machinery the 2026-08-05 trace below
guessed was responsible. `$scheduler.cue(...)`, `Promise.start`, and
`Thread.start` are METHOD calls / native dispatch, not the bareword `start`
function, so they never took this path and inherited the staleness.

## What was tried on 2026-08-15 and why it was reverted

Three fix designs were implemented and tested; **all three were reverted**
because each one fixed the reported bug but broke something else that CI
(or a locally-run related test) caught. This section is deliberately
detailed so the next attempt does not have to rediscover these hazards from
scratch.

### Attempt 1: blanket `sync_env_from_locals` inside `clone_for_thread_excluding`

Moved the equivalent of the old `start`-only blanket flush into
`clone_for_thread_excluding` itself (called by every spawn path), using
`self.current_code` (an `unsafe` raw pointer to the live bytecode frame's
`CompiledCode`, the same pattern `atomic_scalar_cell` already uses) to reach
the calling frame's `locals`/`CompiledCode` from inside that shared
function.

**Broke `Cro::Core/tcp.rakutest`'s `test-connector-nodelay` subtests**: it
HUNG (120s CI timeout, reproduced locally too, confirmed via `rust-gdb`
thread backtraces / `perf record` on the hung process). Root cause: flushing
EVERY local of the calling frame into `env` — including a
`Cro::TCP::Listener`/`Cro::Service` object local that had NEVER been
mirrored into `env` before — made that local newly visible to
`clone_for_thread_excluding`'s existing handle-tracking walk
(`referenced_handle_ids`, further down the same function), which THEN
`try_clone()`'d (OS-level `dup()`) its socket FD into the spawned thread's
own `io_handles` map. That is an ownership change nothing else in the
codebase expects — a SECOND thread ended up racing the listener's own
accept loop for incoming connections, and the test's own expected
connection routing broke, hanging on a `Channel.receive()`/`await` that
never got its value.

### Attempt 2: scope the refresh to the closure's own free-var / atomic-target names

Narrowed attempt 1 to only refresh the SPECIFIC names the spawned closure's
own compiled metadata says it reads (`cc.free_var_syms`, plus a NEW
`CompiledCode::atomic_ref_target_syms` field added specifically to capture
`cas`'s target name — since, per root-cause step 2 above, that name is a
string constant invisible to `free_var_syms`).

**Broke `t/shared-store-lineage-scope.t` test 8** ("the child shadow dies at
block exit; the captured outer value is back") and
**`t/shared-var-lane-param-rebind.t` tests 1-2** ("reduce callback @/%
param is fresh per iteration inside start"). Root cause: the compiler's
free-var analysis is INCOMPLETE for the set of names that actually need
this refresh:
- A name shadowed-then-restored WITHIN THE SAME closure body
  (`start { { my $c = 'shadow'; } $c; }`) is not recorded as a "free
  variable" of that closure at all from the compiler's point of view (the
  outer `$c` reference resolves some other way at runtime), so no free-var
  scan can find it.
- A `reduce -> $h, @words { … start { [+] @words } }` callback's `@words`
  parameter is bound via `bind_param_value`'s env-level binding path,
  which — per `runtime/runtime_thread.rs`'s own `block_captured_scalars`
  comments — has "no local slot behind it" in the destructuring case, but
  in the PLAIN (non-destructured) case DOES occupy a local slot, just keyed
  in `code.locals` WITH its sigil (`"@words"`, not `"words"`) — a
  convention difference from scalars (stored bare, `"a"` not `"$a"`) this
  attempt initially got wrong too, before hitting the more fundamental
  free-var-completeness problem above.

### Attempt 3: shape-filtered blanket sync, scoped to `clone_for_thread_for_block` only

Combined lessons from both: filter by the LOCAL SLOT's VALUE SHAPE (plain
scalar / `Array` / `Hash` / `ContainerRef` only — explicitly excluding
`Instance`/`Package`/`Sub`/`Proxy`, which structurally excludes any handle
regardless of name, fixing attempt 1's hazard) rather than by scanning free
vars (fixing attempt 2's incompleteness), and — because `current_code` is
NOT safely dereferenceable from every `clone_for_thread`/
`clone_for_thread_for_block` caller (see below) — scoped the `unsafe`
`current_code` read to `clone_for_thread_for_block` only, whose exactly two
callers (`spawn_callable_promise` for `start`/`Promise.start`/
`Thread.start`, and `cue_every_timer` for `$scheduler.cue(:every)`) both
dispatch synchronously off a live VM opcode within the same `exec_one` call
that set `current_code`.

This one passed everything checked LOCALLY — the original repro, a new
`t/cue-loop-lexical-reset.t` pin, `t/shared-var-lane-param-rebind.t`,
`t/shared-store-lineage-scope.t`, `start`/`Promise.start` sanity checks, and
**three separate clean runs** of `Cro::Core/tcp.rakutest` (44/44, matching
the pre-fix baseline exactly, confirming attempt 1's hang was genuinely
fixed by the shape filter + narrower scope).

**But CI still failed** (`test`, `gc-stress`, `jit-stress` jobs all red on
PR #6456's second push), and a full local `make test` run (29261 tests)
surfaced a THIRD, different regression not caught by any of the targeted
checks above: **`t/shared-var-nil-redeclared-mask.t` test 6** ("bind stays
Nil across a spawn between declaration and read") — `my $depends :=
nil-returner(); await start { 1 }; $depends;` returned the `Any` type
object instead of `Nil` after the intervening spawn. This is confusing on
its face, because `Nil`/`Any`-shaped values are NOT in attempt 3's shape
allow-list, so `sync_plain_locals_to_env` should not have touched
`$depends`'s own slot directly — **the actual mechanism was not
root-caused before this session's time ran out.** Plausible directions for
whoever picks this up: (a) the sync's write for some OTHER local in the
same frame indirectly perturbs the `thread_redeclared_vars`/shared-store
masking `t/shared-var-nil-redeclared-mask.t`'s own comment describes (the
`set_env_with_main_alias` write path used by the sync calls into
`set_shared_var_sym`, which has its own `thread_redeclared_vars` gate — an
interaction between the TWO gates was never traced), or (b) `victim-spawn-
between`'s `code.locals` genuinely contains something the shape filter
does not exclude that shares/aliases the same underlying storage as
`$depends`.

Given a full `make test` run was needed to surface this third hazard (none
of the ~45 targeted `t/*.t` files this investigation ran manually caught
it), and CI's `gc-stress`/`jit-stress` jobs failed too (not yet
individually triaged against attempt 3 — could be the SAME root cause as
the `Nil`-mask failure, or a fourth, still-undiscovered one), **all three
attempts were reverted** rather than landing a fix with a known-but-
unexplained correctness hole.

## Why this needs more than one more session

Three independent attempts, each fixing the previously-discovered hazard
and introducing (or leaving unfixed) a new one, is a strong signal that
`clone_for_thread`/`clone_for_thread_for_block`/`clone_for_thread_excluding`
sit at the intersection of at least three separate correctness concerns
that do not currently have a unified model:

1. **Handle/FD ownership** (`referenced_handle_ids`, `io_handles` transfer)
   — must never be triggered by an incidental env-visibility change.
2. **The env-mirror gate's completeness** (`compute_needs_env_sync` /
   `needs_env_sync`) — is genuinely incomplete for `cas`-only locals
   (by design, per the `n != "__mutsu_cas_var"` guard) and for
   shadow-restore/env-bound-parameter locals (not by explicit design, just
   an analysis gap), and no single closure-local metadata scan currently
   captures the full set of names that need refreshing.
3. **The `thread_redeclared_vars`/shared-store masking** that keeps a
   later, unrelated same-named lexical from seeing a foreign thread's
   value (`t/shared-var-nil-redeclared-mask.t`'s whole reason for
   existing) — whose interaction with a NEW write path (this ticket's
   fix) was never traced to completion.

A real fix needs either (a) a session with a much larger time budget that
starts with `make test` running continuously in the background while
iterating (so hazard #3's class of regression is caught within the same
session it's introduced, not after review handoff), or (b) — more likely,
given three attempts already converged on "shape-filter what gets
refreshed into `env`" as directionally correct but each left a gap — a
proper ADR-level design pass that enumerates the full set of "who reads a
local by name across a thread-spawn boundary" cases up front (cas/atomic
targets, shadow-restore, env-bound aggregate parameters, and whatever
`shared-var-nil-redeclared-mask.t` is actually guarding against) rather
than discovering them one CI-red-then-revert cycle at a time. This is the
"ADR-0010/Track-B-adjacent design work" the 2026-08-05 trace below already
anticipated, now with three concrete, characterized failure modes to design
against instead of a hypothesis.

## Root cause, as traced 2026-08-05 (superseded by the analysis above —
## kept for history; the "binding-generation" theory was never actually
## the mechanism, but the observed symptom description is still accurate)

The `cas $a` inside the cue callback does NOT go through the name-keyed
shared-variable lane (`shared_vars` stays `0` throughout per the original
debug trace) — it goes through a SEPARATE mechanism, the bare-name
atomic-key machinery (`reset_atomic_var_key_decl` / `__mutsu_atomic_*`
keys). For a LOOP-BODY lexical, the redeclaration reset doesn't fully take:
the next iteration's `my $a = 0` runs `reset_atomic_var_key_decl`, but the
(still-running, or freshly re-cued) callback thread's writes re-create/
overwrite the bare-name atomic entry, and the new iteration's fresh `$a`
slot resolves back to that stale entry. Per the ticket: block-scoped
versions (`{ my $a = 0; ... }` as a sequence of separate blocks, not a
loop) are mostly unaffected because each block's declaration +
`thread_redeclared_vars` mask + `clone_for_thread`'s `declare` reseeds the
lane properly — the LOOP-body slot path specifically skips part of that
reseeding (even though `env.get("a")` at cue-time correctly shows `0`, the
callback thread still resolves the stale atomic entry, not the fresh one).

**2026-08-15 correction**: `reset_atomic_var_key_decl` was directly
verified (via `rust-gdb`) to work correctly — it removes the right
`__mutsu_atomic_name::a` / `__mutsu_atomic_value::N` entries from the ROOT
shared store every round, and every round DOES get a genuinely fresh
`__mutsu_atomic_value::N` key (confirmed `::1`, `::2`, `::3`, … via
instrumentation). The actual leak is the `env["a"]` staleness described in
detail above, not a failure of the atomic-key reset itself.

## Impact

- `:every` cues (and plausibly taps/timers) whose callbacks `cas`/atomically
  mutate a loop-body lexical accumulate across iterations.
- The `.cancel` bounded wait added in ADR-0020 slice 2 hides the common roast
  shape (block-sequenced cues, `LEAVE $c.cancel`), so
  `roast/S17-scheduler/every.t` is stable — but the loop shape above remains
  wrong deterministically.
