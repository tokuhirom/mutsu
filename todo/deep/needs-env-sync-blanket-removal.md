# The `needs_env_sync` / `captures_env_by_name` blanket, and why removing it is a fused campaign

Extracted from PLAN.md §5 (2026-08-02) so the analysis survives PLAN.md's slimming. This is the
last structural piece of the dual-store (locals ↔ env) decoupling, and it has a track record of
breaking several unrelated mechanisms when attempted as a standalone change.

## Root cause

`captures_env_by_name` is true if a frame contains even one of
`ForLoop` / `BlockScope` / `BlockLocalScope` / `MakeGather` / `WheneverScope`. When it is true,
**every local in that frame becomes an env-mirror target**, so locals that are never read by name —
a loop body's `my $ts`, an accumulator — are still written to env on every store. It is a
per-*frame* approximation of a per-*slot* property.

The *per-store* half of the problem is already gone: the `exec_set_local_op_inner` tail env write
for plain lexicals was gated permanently by the `(B)` gate (#4942 flip → #4980 removal, now
unconditional for `!captures_env_by_name` frames), which is why `while` / `loop` bodies already
benefit (-10–16% on a JIT-bailed 5M-iteration while loop). What remains is precisely the blanket.

## Why it is large

The immediate payoff for the common case is *small*, and the mechanism is fused with several
others:

- A `for` body compiles **inline into the same frame** as the `ForLoop` op (`stmt.rs` emits the
  body ops right after `ForLoop`; `body_end` marks the range), so the body's accumulator slots get
  blanket-synced — yet simply dropping `ForLoop` from `captures_env_by_name` measured **±0%**,
  because the loop mechanism writes the loop variable to *both* slot and env
  (`vm_for_loop_body.rs:243/251`) and a pure arithmetic body reads its accumulators from slots, not
  env. The lever is the fused precise-ification (per-slot, not per-frame) of the block-restore /
  loop / gather / whenever / closure-capture consumers — not a `ForLoop`-only drop.

- A 2026-07-15 probe gated the *unconditional* env write at the tail of `exec_set_local_op_inner`
  (`vm_var_assign_set_local.rs`, `set_env_plain_lexical` / `set_env_with_main_alias`) on
  `needs_env_sync || reflective`. Note this tail write, **not** `flush_local_to_env`, is the real
  per-store cost — `flush_local_to_env` is already gated on `needs_env_sync`, so the `env_flushes`
  counter reads 0 and never surfaces it (measure by wall-clock). The gate won ~7% on a JIT-bailed
  `time-parts` loop and **deterministically broke four independent mechanisms**, each pinned by an
  existing test:

  1. **Block-scope restore.** `exec_block_scope_op` reverts `self.locals` to the pre-block snapshot
     and then **re-pulls every local from env by name**, so without the env seed an outer variable
     mutated inside a bare `{ }` reverts to its pre-block value (`my $x=1; {$x=2}; say $x` printed
     `(Any)`). `BlockScope` / `BlockLocalScope` frames therefore still need the blanket. A loop-body
     `if { }` stays *inline* (no `BlockScope`) unless the branch declares its own `my`
     (`BlockLocalScope`), so most hot loops are unaffected.
  2. **Cross-thread closure capture / `cas`.** A `%h` captured by a `Thread.start` body and mutated
     via `cas` needs its shared-variable cell established through env by name; the gate lost it
     (`tests/gc_stress.rs::dead_sweep_bounds_threaded_mutation_memory`, sum=2 vs 800). Folding
     `closure_compiled_codes` free vars + `op_arg_sources_idx` (rw-arg sinks) +
     `op_container_mutate_const_idx` into `needs_env_sync` fixes this axis.
  3. **Method-call caller-local coherence × JIT inline `GetLocal` (the decider).**
     `tests/jit_diff.rs::hot_method_body_compiles_and_matches` (`my $c=…; for ^30 { $c.bump() }`).
     The earlier "the JIT reads the outer lexical from env" diagnosis was **wrong** — proven by
     unconditionally skipping only `$c`'s env write: the JIT run still prints the right answer,
     because `$c` is read via `GetLocal(0)` from its slot. The real gatekeeper is **method-call
     specific** (a positional *sub* call in the same JIT-hot loop is fine): the method path keeps
     caller-local coherence through env (`vm_call_method_ops.rs`
     `drain_and_reconcile_after_cached_call`), so once the gate makes `$c` env-absent, the first
     `bump()` leaves `$c`'s slot in a state the JIT's Tier-B **inline** `GetLocal` (which bypasses
     `exec_get_local_op`) reads as `Any`, while the interpreter's `exec_get_local_op` still reads it
     correctly — so it surfaces only under `MUTSU_JIT_THRESHOLD=1`. Fixing it is load-bearing
     method-dispatch work.
  4. **Currying / priming capture.** `roast/S06-currying/positional.t` aborts at test 157 (cause not
     isolated; likely the same method-dispatch env reconcile as (3)).

So this is a campaign fused with the lexical-slot work
([docs/lexical-scope-slot-campaign.md](../../docs/lexical-scope-slot-campaign.md), whose core move
is removing the `self.locals.clone()` in `exec_block_scope_op`) and with the method-dispatch
env-based caller-local reconcile.

## Notes for whoever picks it up

- Scalar locals are stored **sigil-less** (`"c"`, not `"$c"`) — relevant when instrumenting, and the
  reason a probe keyed on `"$_"` never fires for the topic (its env key is `"_"`).
- The probe above was clean-reverted; there is no half-landed state to unwind.
- Priority: this is perf polish. mutsu already beats raku on the whole roast whitelist and on every
  bench; confirm it unblocks a goal item before starting.
