# Fixed: inline `start` blocks clobbering a later-declared variable

```raku
my $p = Promise.allof(start { 1 }, start { 2 }); await $p; say $p.WHAT;
# was: Nil        now: (Promise)
```

This bug tracked in `todo/deep/inline-start-blocks-clobber-a-later-declared-variable.md` across four
investigation rounds (2026-08-14 through 2026-08-19) is now fixed.

## Root cause

`start { ... }` blocks passed inline as call arguments (e.g. as `.allof`'s arguments) spawn BEFORE the
variable they will eventually be assigned to (`$p` above) has its real value — at that point `$p` only
holds `SetVarDynamic`'s hoisted pre-assignment placeholder (Nil). The pre-spawn sync that runs before
every `start` call (`sync_env_from_locals`) published EVERY local in the calling frame into the
cross-thread `shared_vars` store unconditionally, including `$p`'s stale placeholder, marking it dirty.
`$p`'s real assignment then skipped its own cross-thread mirror (an existing, otherwise-correct
optimization: a slot no code reaches by name doesn't need one), so the stale dirty entry survived. Any
later `await` — of anything, not necessarily `$p` — pulled the dirty store back into `env` and from
there into `$p`'s own local slot, clobbering the correct `Promise` with the stale `Nil`.

## The fix

Gate the pre-spawn cross-thread publish on the same signal (`needs_env_sync`, a compiler-computed
per-slot "does any code reach this local by name" flag) the ordinary per-store write path already
trusts, via a new `sync_env_from_locals_needed` used only at the two "before start spawn" call sites
(`src/vm/vm_call_func_ops.rs`). The plain `env` mirror stays unconditional (only the `shared_vars`
cross-thread publish is gated) — an earlier attempt that also gated the env write reopened a different
hole (a declaration-time env placeholder that's never corrected once its own store skips its mirror).

The harder half: `cas`'s target variable reaches its builtin as a **string constant**
(`cas $var, ...` → `__mutsu_cas_var("var", ...)`), invisible to the ordinary free-variable op scan that
`needs_env_sync` was built from — so a `start` block that RMWs a captured scalar via `cas` needs its
own dedicated recognition. Rather than folding `cas` targets into the existing `free`/`free_writes` sets
(which would also change closure capture/cell-promotion decisions — `cas` deliberately stays on its own
name-keyed cross-thread lane, not the cell-promoting one, per an earlier decision), a new
`CompiledCode::rw_arg_env_sync_syms` field records every rw-arg-sink target name as a side channel, fed
into `needs_env_sync` only, and bubbled transitively through nested closures in `compute_free_vars` up to
the frame that owns the local.

Notably, gating `apply_pending_caller_var_writeback` (the mechanism that pulls a reconciled cross-thread
value back into the caller's own slot) turned out to be unnecessary and actively harmful — an earlier
investigation round added such a gate believing it was required, which broke the atomic-scalar-cell
subsystem's own independent cross-thread visibility mechanism (shared RMW cells with no per-name publish
once a cell exists). With the pre-spawn publish now correctly suppressed at the source, the writeback
never receives a stale value to apply, so it stays fully unconditional, exactly as before this fix.

## Verification

New pinned test: `t/start-block-inline-arg-locals-clobber.t` (both of this bug's original repro shapes,
plus a standalone `cas`-in-`start` case, plus the specific two-block sequencing that exposed the
`cas`-completeness gap). Also verified: the full `t/` suite, `t/lock.t` / `t/promise-combinator.t` /
`t/cross-thread-shared-var-writeback-coherence.t` / `t/atomic-scalar-follows-its-binding.t` (concurrency
regression suite), and `roast/S17-{promise,lowlevel,scheduler,channel}/*.t` plus
`roast/S12-construction/roles-6e.t` (the closure/role capture case an earlier, reverted attempt at this
fix had broken).

## What's still open

- `expr_needs_interpreter`'s blanket `"start"` → tree-walk-fallback exclusion (blocking OTF compilation
  of any `start` block) is a separate problem needing per-call capture cells, not fixed here — see
  `todo/deep/start-block-otf-compilation-gate.md`.
- Two smaller findings from the investigation are filed separately: `cas`'s delta-lambda compile-time
  rewrite is dead code (perf-only, `todo/tickets/cas-delta-lambda-rewrite-is-dead-code.md`), and an
  asymmetry in which container shapes (Array/Hash refused, Seq/Slip accepted) get promoted into shared
  cells (`todo/tickets/atomic-cell-shape-refusal-asymmetry.md`).
