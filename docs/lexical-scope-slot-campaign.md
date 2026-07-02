# Lexical-scope shadow slots — §1.4/§1.5/§1.3 campaign

Status: **in progress** (started 2026-07-02). Tracks ANALYSIS.md §1.4 (lexical
scope in `alloc_local`), §1.5 (name-based runtime slot resolution removal), and
their unavoidable coupling to §1.3 (env↔locals dual store).

## Goal

Give a shadowing inner-block `my $x` its **own** local slot (instead of sharing
the outer `$x`'s slot), and stop the runtime from resolving a variable name → slot
by searching `code.locals` at run time. The end state removes the whole-`locals`
clone in `exec_block_scope_op` (`vm/vm_misc_scope.rs`), the campaign's headline
cost.

## Why it is one campaign, not one slice

`declare_local` today calls `alloc_local` (get-or-create by name), so a nested
`my $x` reuses the outer slot; shadowing is only correct because the runtime env
fallback restores it (`BlockScope` clones the entire `locals` array on every block
entry/exit). Activating distinct shadow slots makes a single **name** occupy
several `code.locals` slots, which breaks every runtime name→slot resolver.

The census (2026-07-02) found **~120 name→slot resolution sites**, and — the load-
bearing finding — the breakage reaches the **foundation**, not just leaf writebacks:

- `env` is a name-keyed `HashMap<name, Value>`; it cannot hold two live `$x`.
- `BlockScope` exit restore (`vm_misc_scope.rs:333-379`) collapses duplicate
  names to one env value.
- `run_inner`/`run_reuse` init (`vm_run_loop.rs:106/344`) seeds every slot from
  env **by name**.

So full §1.4 activation ≡ replacing the name-keyed env-as-source-of-truth with
slot-indexed locals ≡ §1.3 dual-store elimination.

## Phasing

1. **§1.5 — bake compile-time slots into writeback IR (behavior-preserving).**
   Convert each name-based runtime slot resolver to carry the compiler-resolved
   slot in the opcode/IR. While shadows still share slots this is invisible, but
   it shrinks the flip's blast radius one site at a time and each PR is
   CI-green-verifiable. **← current phase.**
2. **§1.4 — flip `declare_local` to allocate shadow slots** + restore the outer
   binding in `pop_local_scope`. Only sound once every writeback site from step 1
   is slot-baked.
3. **§1.3 — make locals slot-indexed** (drop the name-keyed env as the locals
   source of truth) and remove the `exec_block_scope_op` `locals.clone()`.

## Census — name→slot resolvers (blast radius)

Grouped; `position` = first/outer slot, `rposition` = last/inner slot, `enumerate`
broadcast = touches every slot with the name.

- **Root resolver:** `vm/vm_env_helpers.rs:1004` `find_local_slot` (`position`) +
  wrappers `update_local_if_exists` (:1008, ~80 callers), `locals_get_by_name`
  (:1014), `locals_set_by_name` (:1019).
- **RMW chokepoint:** `vm/vm_var_assign_typed.rs:734` `store_named_scalar_rmw_result`
  (`update_local_if_exists` + `code.locals.position` at :769 for `local_bind_pairs`).
- **SmartMatchExpr** (`s///`/`tr///` topic writeback): `vm/vm_smartmatch_ops.rs`
  uses `lhs_var` (name); opcode `opcode.rs:335`. **← first slice: add `lhs_slot`.**
- **`:=` bind:** `vm/vm_var_assign_local.rs:672` `resolve_pending_alias_binds`
  (`position`), `local_bind_pairs` (`runtime/mod.rs:1531`).
- **rw-arg / undefine writeback:** `pending_rw_writeback_sources` (by name),
  drained via `apply_pending_rw_writeback` (`vm_env_helpers.rs:841`, ~40 sites).
- **param-slot precompute (compile-time, already "bakes"):** `opcode.rs:2649/2673`
  `precompute_param_local_slots` — uses `position`; switch to the compiler's
  scope-correct slot.
- **BlockScope restore sweeps:** `vm/vm_misc_scope.rs:203/238/258/333/367`.
- **env↔locals init/coherence:** `vm_run_loop.rs:106/344`, `vm_env_helpers.rs`
  `sync_env_from_locals`/`sync_regex_interpolation_env_from_locals`/
  `writeback_match_locals`, plus the closure/call-frame capture loops.
- **misc leaf sites:** loop var (`vm_for_loop_dispatch.rs:278`), element/index
  assign, computed-attr, hyper writeback, mixin (`vm_mixin_does_ops.rs:343`),
  `$OUTER::`/`MY::` pseudo-stash — full list in the census (session 2026-07-02).

## Slices (checklist)

- [x] **S1 — SmartMatchExpr `lhs_slot`.** Compiler bakes the LHS scalar var's slot
      (`local_map` lookup at emit time); the `$x ~~ s///`/`tr///` modified-topic
      writeback writes `self.locals[slot]` directly, falling back to by-name only
      for a `None` slot (global LHS / EVAL-carrier outer lexical). Behavior-
      preserving today. *(done)*
- [x] **S2 — RMW chokepoint (postfix `$x++`/`$x--`).** `PostIncrement`/`PostDecrement`
      carry a compile-time `Option<u32>` slot; it is threaded to
      `store_named_scalar_rmw_result`, which mirrors the new value into the baked
      slot and uses it as the `local_bind_pairs` source instead of the by-name
      `code.locals` search. The env-by-name RMW *read* is left as-is (a §1.3
      dual-store concern, not a name→slot ambiguity). `None` for non-local targets
      (`our`/dynamic/temp-value/`AtomicCompoundVar`), which keep the by-name path.
      Prefix `++`/`--` and compound-assign-on-local are still by-name — S2b. *(done)*
- [x] **S3 — prefix `++$x`/`--$x`.** `PreIncrement`/`PreDecrement` gain the same
      compile-time `Option<u32>` slot; `exec_pre_increment_op`/`exec_pre_decrement_op`
      mirror the new value into the baked slot instead of `update_local_if_exists`.
      Same pattern as S2. Behavior-preserving today. *(done)*
      - Note: prefix `++$a` where `$a` is `:=`-bound does NOT propagate to the alias
        (`my $b := $a; ++$a` leaves `$b` stale) — a **pre-existing** bug (the prefix
        path lacks the `local_bind_pairs` propagation that the postfix RMW chokepoint
        has). Out of scope for the slot-baking; noted for a later fix.
- [ ] S4 — `:=` bind: bake source/target slots at emit time instead of
      `resolve_pending_alias_binds` name lookup.
- [x] **S4 — param-slot precompute.** The compiler bakes the positional-param →
      local-slot map into `CompiledCode::param_local_slots` at emit time (from
      `local_map`, right after the param `alloc_local` loops, before the body can
      shadow a param — so it stays the parameter binding slot once §1.4 gives a
      body `my $x` its own slot). `CompiledFunction::precompute_param_local_slots`
      uses the baked list instead of searching `code.locals` by name, falling back
      to the by-name search only for hand-built chunks that never recorded it.
      Verified byte-identical to the old `position` search across the whole
      `make test` suite via a temporary `debug_assert_eq!` (never fired). *(done)*
- [x] **S5 — for-loop scalar-list source writeback.** `for ($a, $b, $c) { $_++ }`
      (and the `<-> $v` rw-param form) writes each mutated loop value back to its
      source scalar; `write_back_to_source_var` resolved the target via
      `update_local_if_exists` (name search). The compiler now bakes a
      `source_var_locals: Vec<Option<u32>>` (parallel to `source_var_names`) into
      the `ForLoop` opcode/`ForLoopSpec` from `local_map`; the writeback writes
      `self.locals[slot]` directly, falling back to by-name only for a `None`
      (an `our`/global/undeclared target). Verified byte-identical to the old
      by-name resolution across the whole `make test` suite via a temporary
      `debug_assert_eq!` (never fired). *(done)*
- [ ] S6+ — remaining leaf `update_local_if_exists`/`find_local_slot` sites: the
      for-loop *container* source writeback (`write_back_container_source`,
      `write_back_for_topic_item`) still resolves its source by the runtime-derived
      `container_ref_var` name (a §1.3 dual-store concern, not a compile-time slot);
      the `single_array_source` live-array re-read (`vm_for_loop_dispatch.rs`);
      element/index-assign, computed-attr, mixin, delete, and the ~80
      `update_local_if_exists` callers generally.
- [ ] §1.4 flip + `pop_local_scope` restore.
- [ ] §1.3 slot-indexed locals + drop BlockScope clone.
