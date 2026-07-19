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
- [x] **S7 — `%h<k>:delete` / `@a[i]:delete` container writeback.** `DeleteIndexNamed`
      gains a compile-time `Option<u32>` slot; the four container write-back sites in
      `vm_var_delete_ops.rs` (the fast hash-delete slot check, the `:=`-cell restore,
      and the two post-delete slot syncs) now go through the new shared helpers
      `resolve_local_slot` / `write_local_slot_or_name` (`vm_env_helpers.rs`) with the
      baked slot instead of `find_local_slot` / `locals_set_by_name` by name. These
      two helpers are the reusable slot-preferring primitives for the remaining leaf
      sites. Behavior-preserving today. *(done)*
- [x] **S8 — `$x does R` in-place mixin writeback.** The `DoesVar` opcode gains a
      compile-time `Option<u32>` slot (baked from `local_map` at emit time, for the
      `$x`/`@a`/`%h` target); `exec_does_var_op` mirrors the mixed-in value into the
      baked slot via the S7 `write_local_slot_or_name` helper instead of
      `update_local_if_exists` by name. Behavior-preserving today; verified
      byte-identical across the whole `make test` suite via a temporary
      `debug_assert_eq!` (never fired). (The pre-existing `@a does R` / `%h does R`
      `.name`-doesn't-resolve gap is unrelated — identical on `main`.) *(done)*
- [x] **S9 — `for @a` live-array source re-read.** The `ForLoop` opcode / `ForLoopSpec`
      gain a compile-time `single_array_source_local: Option<u32>` (baked from
      `local_map`, mirroring the VM's bare-then-`@`-sigiled resolution order). The
      live-array continuation re-read (`vm_for_loop_dispatch.rs`) resolves the source
      via the S7 `resolve_local_slot` helper with the baked slot instead of
      `find_local_slot` by name, keeping the `@`-name + env fallback for a `None`
      slot. Behavior-preserving today; verified byte-identical across the whole
      `make test` suite via a temporary `debug_assert_eq!` (never fired). *(done)*
- [x] **S11 — element/index-assign target slot** (`$a<x> = 42` / `$a<a b c> »=» 42`
      on a shadowed `$a`; also `@a[i] = v`). `IndexAssignExprNamed` gains a compile-
      time `target_slot` (baked at its 6 emit sites: `expr_method.rs` ×2,
      `expr_closure.rs` ×2, `expr_ops.rs`, `expr_call.rs`). `exec_index_assign_expr_
      named_op_inner` computes `eff_slot`/`orig_slot` (gated on `shadow_slots_active()`,
      cleared to `None` when a sigilless alias redirects to a different variable) and
      routes its 7 *target-var* resolvers (3 `find_local_slot` → `resolve_local_slot`,
      4 `update_local_if_exists`/`original_var_name` → `write_local_slot_or_name`)
      through the baked slot; the `source_name`/`src` `:=`-cell sites keep by-name.
      **GATED on `shadow_slots_active()`** because `resolve_local_slot` treats an
      out-of-range baked slot as "not local" (returns `None` instead of a by-name
      search), which diverged on chained slice-lvalue assignment; OFF stays
      byte-identical. `roast/S13-overloading/metaoperators.t` #14-16 green ON.
      Validated OFF with `make roast` (map.t/hash.t transient failures were CPU-
      starvation flaky, not this change). *(#4090-pending)*
- [x] **S12 — env-broadcast duplicate-name skip + for/given container-writeback
      slot bake.** Root cause of the `reverse.t`/`hash.t`/`native-str.t` toggle-ON
      regressions: the whole-locals env broadcasts (`sync_env_from_locals`, run
      unconditionally by `Say`/`Put`/`Print`/`Note`, and
      `sync_regex_interpolation_env_from_locals`) push EVERY slot into the
      name-keyed env, so with shadow slots a later same-named sibling slot
      (uninitialized `Nil`, or a stale copy) clobbers the live value — reads
      (`GetArrayVar`/`GetHashVar` are env-first) then observe it. Fix:
      `CompiledCode::dup_named_locals` (computed in `compute_needs_env_sync`)
      flags every slot whose name occupies >1 slot; both broadcasts skip flagged
      slots (the per-write `flush_local_to_env` mirror keeps env tracking the
      live slot). Intrinsically byte-identical OFF — without the gate,
      `alloc_local` get-or-creates by name so no duplicates exist. Also bakes a
      compile-time slot onto `TagContainerRef`/`TagContainerRefReversed`
      (`container_ref_var` is now `(name, Option<slot>)`) and threads it through
      `write_back_for_topic_item`/`write_back_container_source`/
      `write_back_given_topic` (gated ON), so the `$_ = ++$i for @a.reverse`
      rebuild reads and writes the shadowed source's own slot instead of the
      by-name `position` (outer) slot. ★**Rejected approach (tried first): a
      baked-slot-first READ on `GetArrayVar`/`GetHashVar`.** It fixed the same
      files but broke 4 `t/` files ON (`shared-array-push`, `shared-elem-assign`,
      `slurpy-is-raw`, `subscript-adverbs`): many writeback paths (rw-arg drain,
      pair-lvalue `.value =`, cross-thread `__mutsu_atomic_arr::` stores) still
      write env-only, so a slot-first read observes stale slots. Reads must stay
      env-first until §1.3 makes writes slot-complete — fixing the broadcast
      clobber at its source is the sound slice. Pin:
      `t/shadow-slot-env-broadcast.t` (passes OFF, ON, and real raku).
      *(this branch)*
- [x] **S13 — closure-capture slot bake + shadow cell trigger (§1.3 slice 2:
      "make closure capture slot-addressed").** Root cause of the `wrap.t`
      toggle-ON failures (tests 63/66): a closure passed as a CALL ARGUMENT
      (`.wrap({...})`, `@cs.push({...})`) is classified non-escaping by the
      escape analysis, so `box_captured_lexicals` never gives its
      captured-and-mutated lexical a shared `ContainerRef` cell; the non-cell
      coherence path then writes the mutation back BY NAME (position = the
      OUTER slot), so a closure over an inner shadow updated the wrong slot.
      Two coupled fixes, both gated on `shadow_slots_active()`:
      1. **Emit-point capture slots.** `Compiler::add_closure_code_baked` (the
         single `add_closure_code` chokepoint, 6 call sites) bakes the parent
         `local_map` slot for every child `free_var_syms`/`upvalue_syms` entry
         into new `CompiledCode::free_var_parent_slots`/`upvalue_parent_slots`.
         The four runtime capture resolvers (`capture_closure_env` ×2,
         `capture_upvalues`, `box_captured_lexicals`) go through the shared
         `resolve_capture_slot` helper: baked slot first (validated against the
         slot name), `rposition` fallback. This also fixes the latent
         pre-shadow hazard — `rposition` always picks the INNERMOST same-named
         slot, wrong for a closure created before/outside the shadow block.
      2. **Shadow boxing trigger (path C).** A captured-and-mutated scalar
         whose baked slot is dup-named (`code.dup_named_locals`, S12) gets a
         cell REGARDLESS of the escape analysis — by-name writeback cannot
         disambiguate duplicate names, and the cell mechanism is the sound one
         (per the roles-6e.t lesson). The early-return gate gains a matching
         `dup_shadow_possible` arm. OFF is byte-identical: no duplicate names
         exist, `resolve_capture_slot` reduces to the old `rposition`.
      ON green: `S06-advanced/wrap.t` (90/90), and `S02-types/whatever.t` +
      `S02-types/hash.t` now pass ON too (same capture root cause). `perl.t` #7
      (AssignExprLocal `@`/`%` parity) remains, plus pre-existing
      `outer-topic.t` #4. Pin: `t/shadow-slot-closure-capture.t` (OFF, ON, and
      real raku). *(this branch)*
- [x] **S14 — `$OUTER::name` slot bake.** Under shadow slots, `get_outer_var`'s
      inline nested-block path searched `code.locals` by `position` (the
      OUTERMOST same-named slot), so `$OUTER::a` at depth 1 of a triple shadow
      returned the outermost binding (`variables-and-packages.t` #10/#15; the
      depth-2 `$OUTER::OUTER::a` only passed by coincidence). The compiler now
      resolves the emit-point slot of the binding visible `depth` scopes out by
      unwinding the `local_scopes` shadow records (`resolve_outer_var_slot`:
      each frame that declares the name stores its pre-declaration slot, so
      unwinding the frames deeper than the target scope yields the target's
      slot; `None` when the depth crosses the frame boundary or the name is
      first-declared deeper) and bakes it onto `GetOuterVar`. The runtime
      (gated ON) returns the LIVE `self.locals[slot]` — with shadow slots each
      binding owns its slot, so the live value IS the outer binding, matching
      raku's live-binding semantics with no snapshot indexing at all; OFF stays
      on the existing snapshot/env paths byte-identically. ON green:
      `variables-and-packages.t` 39/39 AND `t/outer-topic.t` #4 (the
      `$OUTER::_` read — the toggle-ON `prove t/` suite is now fully green).
      Pin: `t/shadow-slot-outer-var.t` (OFF, ON, real raku). Known pre-existing
      default-build gaps left as-is (they fail OFF on main identically):
      depth-2 access in a second block group, `$OUTER::` from a block that
      doesn't declare the name, and `$OUTER::` seeing post-entry mutations
      (raku reads the live binding; the OFF snapshot path reads block-entry
      values). *(this branch)*

- [x] **S15 — `(state @foo) = @bar` paren-decl assignment must stay by-name.**
      Under shadow slots the parenthesized-declaration lvalue store
      (`__mutsu_assign_callable_lvalue` branch 1, from #4086) emitted
      `AssignExprLocal(slot)` — but a state AGGREGATE lives in a shared
      `ContainerRef` cell (StateVarInit) aliased by the persisted state store,
      and `exec_assign_expr_local_op` deliberately REPLACES the slot for
      `@`/`%` targets (whole-reassignment semantics; cell write-through is
      scalar-only, `vm_var_assign_local.rs`). The replace detached the slot
      from the persisted cell, so the next call restored the stale cell and
      the per-call reassignment was lost (`state.t` #13 ON: second call
      returned the mutated previous contents). Fix: when the paren-decl is
      `is_state`, always emit the by-name `AssignExpr` (what the default build
      emits anyway — byte-identical OFF), which writes through the state cell;
      state shadowing stays coherent via cell identity, not slot position.
      ON green: `S04-declarations/state.t` 46/46. Pin:
      `t/shadow-slot-state-paren-decl.t` (OFF, ON, real raku). *(this branch)*

- [x] **S16 — `ApplyVarTrait` slot bake (`is default` / QuantHash / Buf / Map
      traits on a shadowing declaration).** The earlier survey note guessed the
      hazard was the initializer's `AssignExprLocal` hitting the outer slot via
      an env-only (`SetVarDynamic`) declaration; the actual root cause is
      simpler: `exec_apply_var_trait_op` resolved the declared variable by
      NAME (`locals_get_by_name`/`locals_set_by_name` = `position` = the
      OUTER same-named slot). For `my @a is default(42) = ^10` shadowing an
      outer `@a`, the `default` branch read the OUTER container, tagged it,
      wrote it back to the outer slot AND broadcast it to env — so the
      env-first `GetArrayVar` read returned the outer contents with the inner
      default (`advent2013-day12.t` #11/31-32: slice reads returned 42 for
      every element). Fix: bake `slot: Option<u32>` onto `ApplyVarTrait` at
      the three emit sites (statement-position VarDecl bakes `local_map` right
      after `declare_local`, so it carries the fresh shadow slot; the
      expression-position sites bake what `emit_set_named_var` stored to, or
      `None` for the pure-`SetGlobal` path) and route all eleven declared-var
      resolver sites in `vm_var_trait_ops.rs` through the slot-preferring
      helpers (`read_local_slot_or_name`, a new read mirror of
      `write_local_slot_or_name`, + the existing write helper), gated on
      `shadow_slots_active()` — OFF passes `None` and stays on the by-name
      path byte-identically. ON green: `integration/advent2013-day12.t` 32/32.
      Pin: `t/shadow-slot-var-trait.t` (OFF, ON, real raku — covers
      `is default` on `@`/`%` shadows, `:delete` holes, outer intactness, and
      an `is Bag` QuantHash shadow). Residual (pre-existing, both modes): the
      name-keyed `var_defaults` side table and the `trait_mod:<is>` fallback's
      `env().get(name)` are still by-name — §1.3 territory. *(this branch)*

- [x] **S17 — `(my @x)`/`(my %h)` paren-decl assignment must stay by-name
      (generalizes S15 from state aggregates to ALL aggregates).** Under
      shadow slots the paren-decl lvalue store emitted `AssignExprLocal(slot)`
      for `@`/`%` targets, which REPLACES the slot, while the by-name
      `AssignExpr` assigns IN PLACE through container identity. The circular
      `.raku.EVAL` roundtrip shape `((my @y) = [42, @y])` needs the in-place
      write: the RHS captures the container the declaration just created, and
      the in-place assign makes element 1 that SAME container (a cycle); the
      slot replace left the captured container empty, broke the cycle, and
      `S32-array/perl.t` #7 then aborted downstream indexing `Any` ("Any
      cannot be parameterized" — a red herring; the plan abort was the
      symptom). Fix: extend the `is_state` exception in
      `compile_expr_call`'s paren-decl branch to any `@`/`%` target — always
      emit the by-name `AssignExpr`, which is what the default build emits
      anyway (byte-identical OFF). Aggregate shadowing stays coherent through
      container/env identity (aggregate reads are env-first), not slot
      position. ON green: `S32-array/perl.t` 9/9 — **the last remaining
      toggle-ON failure from the 2026-07-10 full survey; the known ON
      burndown list is now EMPTY** (a fresh full survey is the next step to
      confirm). Pin: `t/shadow-slot-paren-decl-aggregate.t` (OFF, ON, real
      raku — circular roundtrip, RHS self-reference identity, shadow
      correctness for `@`/`%` paren-decls). *(this branch)*

## Fresh full toggle-ON survey (2026-07-10, post-S13, release, 1373 files)

Only **4 genuine ON failures** remain; all were verified pre-existing on `main`
(A/B via `git checkout main -- src/` + rebuild — S13 introduced zero
regressions). Earlier surveys re-probed only the survey-#2 regression set, so
three of these were missed until now — re-run the FULL survey periodically.

- `S02-names-vars/variables-and-packages.t` #10/#15 — `$OUTER::a` × shadow
  (**fixed by S14**, this branch)
- `S04-declarations/state.t` #13 — `(state @foo) = @bar` paren-decl lvalue ×
  shadow (**fixed by S15**, this branch)
- `integration/advent2013-day12.t` #11/31-32 — `@a[2,3,4]` slice + `is default`
  under ON returns the default for all elements
- `S32-array/perl.t` #7 — known (#4086 AssignExprLocal `@`/`%` attr-mirror
  parity; bad-plan abort)
- (`t/` side: only `outer-topic.t` #4, pre-existing)

- [ ] S15+ — remaining leaf `update_local_if_exists`/`find_local_slot` sites: the
      nested/deep/generic index-assign
      variants (`IndexAssignExprNested`/`DeepNested`/`Generic`), computed-attr twigil
      cells, hyper `»=»` multi-key writeback, and the remaining `update_local_if_exists`
      callers — migrated onto `resolve_local_slot`/`write_local_slot_or_name`.
- [x] §1.4 flip + `pop_local_scope` restore — **DEFAULT ON since 2026-07-12.**
      The gate (`shadow_slots_active`) now defaults true; `MUTSU_NO_SHADOW_SLOTS`
      is a temporary opt-out escape hatch. Green light = the fresh full toggle-ON
      survey below.
- [x] **Per-block whole-`locals` clone removal — DONE under shadow slots
      (2026-07-19, slices 1-3, PRs #4818/#4821/#4827).** The three clones the
      campaign existed to delete are gone on the default build:
      - **Slice 1 (#4818):** `exec_block_local_scope_op`'s `self.locals.clone()`
        (BlockLocalScope, the if/unless/else branch path) → a targeted Nil-reset
        of the branch's fresh declarations. A fresh (non-shadow) declaration owns
        a unique slot whose pre-branch value is `Nil` by induction, so resetting
        to `Nil` is identical to restoring a snapshot, at O(declared).
      - **Slice 2 (#4821):** `outer_scope_locals.push(saved_locals.clone())`
        (`$OUTER::` snapshot) → an empty frame under shadow slots. In-frame
        `$OUTER::` resolves through the compiler-baked outer slot against the live
        `locals` (`get_outer_var` shadow fast path); cross-frame resolves against
        the captured `__mutsu_outer::` env. Neither reads the snapshot.
      - **Slice 3 (#4827):** the headline `self.locals = saved_locals` whole-array
        restore → a targeted Nil-reset of fresh block declarations, keeping the
        existing env re-seed (which already holds the correct post-block value for
        every enclosing name). No per-block locals clone is taken at all now.

      The `MUTSU_NO_SHADOW_SLOTS` opt-out still takes the full snapshot (it lacks
      distinct shadow slots and needs the whole-array restore + `$OUTER::`
      snapshot). GC root tracing (`gc_roots.rs`) traces whatever `outer_scope_locals`
      holds, so the empty frames are harmless.

      **Still open — the dual-store half of §1.3.** Slice 3's restore still
      re-seeds enclosing slots from `restored_env` (the name-keyed env), i.e. it
      still relies on the `needs_env_sync` blanket mirroring block-body stores to
      env. Dropping that blanket (the fib env-COW perf payoff) requires making
      block exit slot-authoritative — no `restored_env` re-seed, slot-based
      save/restore for the non-propagating `$_`/`$*dyn` names — while keeping the
      `__mutsu_loop_*` control temps and the `MakeGather`/`WheneverScope` by-name
      bodies env-synced. That is the fused §1.3 + §1.2 campaign below, not a clone
      removal, and it is the remaining work.

## §1.3 is an ARCHITECTURE slice, not a perf slice (measured 2026-07-14)

Two things were established by measurement while chasing the call-path profile
(see [docs/perf-callpath-scouting.md](perf-callpath-scouting.md) §4). Both matter
for how this step is scoped:

1. **The clone is not where the time is.** `exec_block_scope_op`'s
   `self.locals.clone()` does not appear in any benchmark profile. The interpreter's
   real per-op cost was runtime-built `__mutsu_*::<name>` metadata keys (`format!` +
   `Symbol::intern` + SipHash), removed in #4492–#4495 for 24–39% on the benches.
   Do §1.3 because it removes the dual store, not because it is expected to be fast.

2. **The clone removal and the `needs_env_sync` blanket are ONE change, and the
   order is forced.** `compute_needs_env_sync` marks every local of a
   `BlockScope`-carrying frame as env-synced precisely *because* the block exit
   restores the whole `locals` array and re-seeds it from env by name: a block
   body's write to an **enclosing** local only survives the block through its env
   mirror. Narrowing the blanket first is not possible — removing `BlockScope` from
   it deterministically breaks `FIRST`/`NEXT`/`LAST` loop phasers (`t/phasers.t` #3,
   `t/dualstore-slot-local-gate.t` #6: the `LAST` body's `$seq ~= "L"` is reverted at
   block exit). So:

   > bake `block_declared_slots` onto the `BlockScope` opcode → replace the
   > whole-array restore with a targeted reset of just those slots → *then* drop
   > `BlockScope` from the flush blanket.

   Caveat for the last step: the loop-phaser desugaring's control temps
   (`__mutsu_loop_first_*`, `__mutsu_loop_ran_*`, `__mutsu_loop_result_*`, …) are
   threaded through env by name **deliberately** and must stay `needs_env_sync`
   regardless of the blanket. They are recognisable by their `__mutsu_loop_` prefix.

   (`ForLoop` can be dropped from the flush blanket independently — its body is
   compiled into the same chunk, so the per-op scan already sees every by-name read.
   Measured: **±0% on every benchmark**, because the loop benchmarks never reach
   `flush_local_to_env` at all — see the `simple_locals` finding in the scouting
   doc. So it is not worth a PR on its own.)

## Fresh full toggle-ON survey (2026-07-12, debug, 1379 whitelisted files)

`MUTSU_SHADOW_SLOTS=1` (pre-flip) run of every whitelisted file, each ON failure
re-run OFF: **on_fail=113, genuine toggle-ON regressions = 0.** Every file that
fails ON also fails OFF (the on_fail count is inflated by `# TODO` not-ok lines,
which are deterministic and identical ON/OFF). So flipping the default breaks
nothing that currently passes — the §1.5 leaf-slot bakes (S1–S17) plus the
2026-07-10 container-identity/cell campaign drove the §1.3 class-1 (name-keyed
env dual-store) breakage, and every §1.4 survey #1–#3 regression, to zero.
Validated with `make test` under the flip (16351 tests, PASS).

## Clone-removal — DONE (2026-07-19, slices 1-3)

The clone-removal probe (2026-07-12) removed the `self.locals = saved_locals`
restore while keeping only the env-driven re-sync, and regressed
`t/block-lexical-scope.t` #3: `{ our $sa2 = my $sb2 = 42 } ($sa2, $sb2)` must
throw `X::Undeclared`, but without a reset the block-declared slots kept their
in-block value (42) and a post-block reference found the stale slot. That fixed
the design: the restore's second role is to **clear the block's fresh declarations
so they cannot leak past the block**.

The landed fix (slice 3, #4827) does exactly that — a **targeted Nil-reset of the
block's fresh declarations** — but **without** the compile-time `block_declared_slots`
bake the 2026-07-12 note anticipated. That bake turned out unnecessary: the
runtime `block_declared` **name** set plus a "not in `saved_env`" test already
isolates the fresh (single-slot) declarations from shadowing ones (a shadowing
name's outer slot is untouched and its inner slot is dead post-block), reusing
slice 1's technique. `$OUTER::` got its non-snapshot path via slice 2 (the empty
`outer_scope_locals` frame + the existing baked-slot fast path), and `gc_roots.rs`
traces the now-empty frames harmlessly. Validated OFF and ON with `make test`
(18678 PASS) and the CI `make roast`.

**Remaining:** the dual-store half — dropping the `needs_env_sync` blanket so
block exit no longer re-seeds slots from the name-keyed env (the fib env-COW perf
payoff). See the fused §1.3 + §1.2 campaign notes below; that is the open work,
not clone removal.

## Block exit is slot-authoritative — DONE (2026-07-19, PR #4844)

The first step of the "dual-store half": `exec_block_scope_op` no longer re-seeds
**propagating** enclosing slots from the name-keyed `restored_env`. Under shadow
slots every store already writes the slot (`exec_set_local_op_inner`), so a
propagating enclosing var holds its live value in its own slot and the env re-seed
is redundant. The restore now touches **only the non-propagating names**:
block-declared fresh `my` → Nil (folds in the old separate Nil loop), and
block-declared shadowing names / `$_` / `$*dyn` → their saved outer value (still in
`restored_env` because propagation was skipped for them). The
`MUTSU_NO_SHADOW_SLOTS` opt-out keeps the whole-array restore + full re-seed.

Behavior-preserving (make test 18747, scoping/phaser/declaration roast 77/77, pin
`t/block-restore-slot-authoritative.t`), and perf-neutral on its own — it is a
**prerequisite**: it removes ONE of the four env-mirror dependencies (mechanism #1,
block-restore's env pull-back) that block dropping the `needs_env_sync` blanket.
The three still open: cross-thread shared-var copy, method-call caller-local
coherence × the JIT inline GetLocal, and currying/priming capture (see
[[memory: needs_env_sync blanket removal]] — the store-site gate breaks all four
until each is freed). Only after all four are slot-authoritative can the
unconditional per-store env write in `exec_set_local_op_inner` be gated for the
env-COW payoff.

## The `(B)` per-store env-write gate — burndown map (survey 2026-07-19, post-#4844)

`exec_block_scope_op` mechanism #1 is fixed (above), but mechanisms #2-#4 have **no
independent behavior-preserving slice** — they are consumers of the per-store env
mirror that only break when the store `(B)` is gated. So the remaining campaign is
the proven gated-flag methodology (as `MUTSU_SHADOW_SLOTS` was): gate `(B)` behind a
default-OFF flag (byte-identical OFF), then burn down the ON failures by folding each
mechanism's names into `needs_env_sync` (`compute_needs_env_sync`, opcode.rs), then
flip and delete the flag.

**The gate** (reconstruct in `exec_set_local_op_inner`, the `plain_locals` branch —
the write is `set_env_plain_lexical`; keep the term-symbol block and `:=` alias chain
unconditional):

```rust
let skip_env_write = gate_local_env_write()            // MUTSU_GATE_LOCAL_ENV_WRITE
    && !is_bind && !is_constant
    && !code.captures_env_by_name                      // keeps ForLoop/Block/gather/whenever
    && !code.needs_env_sync.get(idx).copied().unwrap_or(true)
    && !crate::opcode::reflective_name_access_possible()
    && Self::term_symbol_from_name(name).is_none();
```

**ON survey (debug, `MUTSU_GATE_LOCAL_ENV_WRITE=1 prove -j4 t/`): 75 failing files**,
clustered by the mechanism whose names must be folded into `needs_env_sync`:

| Cluster | ~files | Root: a name read from env not the slot |
|---------|-------:|-----------------------------------------|
| native ctor / `.Set/.Bag/.Mix`/hash coerce | 14 | native constructor / coercer reads a ctor arg local by name |
| io / json / misc (catch-all; many share the roots left) | 31 | mixed — triage per file after the named clusters shrink it |
| cross-thread / atomic / `cas` / lock | 7 | captured `%seen` etc. loses its env seed → lost-update (gc-stress class) |
| method-call / attr self-writeback | 6 | `drain_and_reconcile_after_cached_call` keeps caller-local coherence via env |
| closure capture / overlay | 6 | `capture_closure_env` / `box_captured_lexicals` reads free vars by name |
| rw-redispatch / lvalue-rw | 5 | `apply_pending_rw_writeback` / nextsame rw-arg drain is by-name |
| `let` / `temp` restore | 2 | scope-exit saved-var restore resolves by name |
| sigilless `\x` params | 2 | caller-alias writeback across EVAL (the §1.1 OTF-gate exclusion) |

**Method:** fold one cluster at a time into `needs_env_sync` (e.g. register the
nested-closure `free_var_syms ∩ own`, the `op_arg_sources_idx` rw-sinks, and
`op_container_mutate_const_idx` for #2/#4; add the method-coherence names for #3),
re-run the ON survey, confirm the cluster's files go green ON while OFF stays
byte-identical, `make test` + gc-stress each time (the cross-thread cluster is
gc-stress-gated and flaky-prone — validate on CI). The `io/misc` bucket (31) is a
catch-all that should shrink substantially as the named clusters are fixed; triage
its residue last. **A dedicated session per cluster.**

### Root-cause drill-down (2026-07-19): the clusters are NOT independent — #3 is the shared root

Diagnosing the "native ctor / coerce" cluster showed its files are really **mechanism
#3** (call-return caller-local coherence), not native constructors. Minimal repro:

```raku
my $l = List.new(1, 2, 3);   # scalar local holding an aggregate
isa-ok $l, List;             # a Test sub call — the trigger
say $l.elems;                # ON: 1  (expected 3) — $l has become Any
```

The chain:
1. The declaration `my $l` seeds `env["l"] = Any` (the decl path writes env before
   the initializer runs).
2. `$l = List.new(...)` is a plain-lexical `SetLocal`; the gate **skips its env
   write**, so the slot becomes the List but `env["l"]` stays `Any` (stale).
3. `isa-ok $l` (a Test fn) dispatches through the **carrier fallback** in
   `exec_exec_call_pairs_op`, whose post-carrier reconcile
   `carrier_writeback_changed_aggregates` (vm_env_helpers.rs) writes a slot back from
   env when the *current* env value differs in variant from the slot — the
   "type change away from container" case (`$a does Role` turning a Hash into a
   Mixin). With the gate ON that read is stale: `env["l"] = Any` (a `Package`) differs
   in variant from the slot's `List` (an `Array`), so it **clobbers the good slot with
   the decl-seed `Any`**.

**Correction (2026-07-19, verified):** the drill-down originally fingered
`apply_pending_rw_writeback` (:1089/:1124). Instrumentation disproved that — on this
repro both of those drain with **empty** source sets; the actual clobber is the
`carrier_writeback_changed_aggregates` "type change away" branch, which did **not**
check that env genuinely changed *during* the carrier (unlike its sibling overwritable
branch, which already diffs against the pre-carrier `pre_env` snapshot).

**Fix (landed):** require `env_changed` (a `pre_env[i] != cur` diff) before the
type-change slot write, matching the overwritable branch. Gate OFF this is
byte-identical — env tracks the slot, so a fired type-change always had
`prev != cur` anyway; gate ON it suppresses the stale-`Any` clobber. Pin:
`t/gate-b-carrier-aggregate-clobber.t`.

**ON-survey delta:** 75 → **61** genuine ON-only regressions (all 61 pass OFF). The
14 files cleared are exactly the "native ctor / `.Set/.Bag/.Mix`/hash coerce"
cluster's carrier-writeback subset (`my $x = <aggregate>; is/isa-ok/ok $x`). The
remaining 61 belong to the other mechanisms (#2 cross-thread, #4 curry, closure
capture, rw-redispatch, `let`/`temp`, sigilless) — those are the next per-cluster
name-folding targets.

### `let`/`temp` cluster — DONE (2026-07-19, structural slot-read fix)

The `let`/`temp` cluster (2 files: `let-temp.t`, `let-temp-restore-writeback-coherence.t`)
was fixed **structurally**, not by folding into `needs_env_sync`. Root cause:
`exec_let_save_op` (vm_misc_block.rs) snapshots the pre-scope value of the temporized
variable by reading `env` by name first, falling back to the slot. Under the gate a
plain-lexical assignment (`my $x = 1`) before the `temp`/`let` skips its env mirror, so
the env-first read snapshots the `my $x` decl seed (`Any`) instead of `1`; the
scope-exit restore then writes that `Any` back. The `LetSave` opcode already bakes the
scalar's slot (`slot: Option<u32>`, index mode excepted), and the restore side
(`restore_let_value`) already prefers it — so the save side now reads the baked slot
first too (the slot is always current; `env` is only a mirror). Gate OFF the slot
equals the env mirror, so this is byte-identical (`make test` 18827 PASS). Pin:
`t/gate-b-let-temp-slot-save.t` (OFF + ON both pass). **ON-survey delta: 61 → 59.**

### `sigilless` cluster — typed-bind read fixed (2026-07-19), EVAL-rw deferred

The 2-file sigilless cluster splits into two independent roots:

1. **Typed sigilless bind read — DONE (structural).** `my Int \d := 7; is d, 7`
   failed ON (`d` read as `(Int)`/`Any`). Root: a *typed* sigilless declaration
   (`type_constraint.is_some()`) was emitted as a bare `VarDecl` with no sigilless
   marker, so `d` never entered the compiler's `sigilless_locals` set and the
   bare-word read compiled to `GetBareWord` (env lookup) instead of `GetLocal`
   (slot). Under the gate the env is stale → `Any`. The untyped case
   (`my \c := 42`) worked because it emits `MarkSigillessReadonly`. Fix: a new
   non-readonly `Stmt::MarkSigilless(name)` marker (typed sigilless keeps container
   mutability, so it must NOT be marked readonly), emitted by the typed `:=` and `=`
   sigilless declaration paths and compiled to `sigilless_locals.insert` only. This
   makes the read slot-addressed regardless of the gate. Gate OFF byte-identical
   (`make test` 18891 PASS); `sigilless-comma-decl.t` goes green ON. (Pre-existing,
   gate-independent: a typed sigilless bind to a container is not yet writable-through
   — `my Mu \a := $x; a = 5` leaves `$x` unchanged in both modes; raku propagates.
   Orthogonal to this cluster.)

2. **EVAL rw caller-alias writeback — DEFERRED (the §1.1 hard case).**
   `sub swap(\x,\y){ my $z=y; y=x; x=$z } my $a=5; my $b=3; EVAL 'swap($a,$b)'`
   still fails ON (`sigilless-params.t` test 3). Narrowed: a *read-only* sigilless
   sub via EVAL works ON (`two(\x,\y)` reads `5|3`); `our`-var args work ON; a
   nested-sub (non-EVAL) swap works ON. Only a **my-lexical arg + rw sigilless param
   + EVAL** breaks — and it breaks the initial *read* too (`x`/`y` empty on entry),
   so the rw param binding routes the caller-lexical alias through the EVAL frame's
   env by name (the `__mutsu_sigilless_alias::` chain), which the gate leaves stale.
   This is the acknowledged §1.1 OTF-gate exclusion; needs the EVAL caller-frame
   alias resolution made slot-addressed. **ON-survey delta for this PR: 61 → 60**
   (sigilless-comma-decl only; sigilless-params stays; the two `let`/`temp` files are
   fixed separately by #4861).
### closure-capture cluster — DONE (2026-07-19), and it collapsed the io/misc bucket

This turned out to be the highest-leverage cluster: fixing its three roots dropped
the ON survey from **61 → 24** (37 files), because most of the `io/misc` catch-all
and `native ctor` files define top-level named subs that read a mainline `my`
lexical — the same root as #3 below. The three roots:

1. **`.map`/`.grep` closure free-var capture — `needs_env_sync` fold (gated).**
   `sub f { my $mul=3; (1,2,3).map({ $_ * $mul }) }` returned `0,0,0` ON. A closure
   handed to the `.map`/`.grep` slow loop is pre-inserted into `self.env` only for
   keys *absent* there (`resolution_map_grep.rs`), so it reads a captured free var
   back from the creating frame's env by name. Under the gate that env is the stale
   decl seed. A structural override (prefer the closure's captured `data.env`) is
   NOT byte-safe OFF — mutsu's compile-time mutation analysis is incomplete, so a
   by-value capture can be stale where the live caller env is fresh (the flaky risk
   ADR-0001/CLAUDE.md warns about). So instead fold every **nested-closure free var
   that is one of this frame's own locals** into `needs_env_sync`, **gated on
   `gate_local_env_write()`** (compute_needs_env_sync, opcode.rs). Gate OFF this is a
   no-op (byte-identical AND perf-neutral — no extra `flush_local_to_env`); the cost
   lands only with the gate and never touches a hot-arithmetic loop local (those are
   not closure free vars).

2. **`state` var in a closure — slot-read, gated.** `-> { state $s=0; $s+=10; $s }`
   stuck at 10 ON. The frame-exit state save-back (`vm_closure_dispatch.rs`) read the
   value from `env` by name first; under the gate that is the pre-update mirror, so
   the state never accumulates. Read the live SLOT first (seeded from the state store
   on entry, updated by the body). This is **NOT byte-neutral** — some state vars are
   mutated only through `env` by name (a `state` referenced from a regex replacement
   part updates env, not the slot — roast `S04-declarations/state.t` #16), so the
   slot-first order is gated on `gate_local_env_write()`; OFF keeps the env-first
   read. (Caught by CI's `make roast` after `make test` alone missed it — the #4086
   lesson: OFF-verify leaf reordering with roast, not just `t/`.)

3. **Named sub reads an enclosing lexical — `needs_env_sync` fold (gated).**
   `my $base=100; sub f { $base+1 }` returned `1` ON. A named sub is registered from
   `stmt_pool` via `RegisterSub` and compiled lazily, so the defining frame cannot
   see which enclosing lexicals its body reads by name, and it reads them from the
   frame's env at call time. Without the sub's free-var set available at
   `compute_needs_env_sync`, conservatively keep **every local of a `RegisterSub`-
   defining frame** env-synced, gated on the flag (OFF byte-identical/perf-neutral;
   the top-level/main frame is never a hot loop). Making this precise (scan the
   registered sub bodies' free vars) is a later refinement.

Gate OFF byte-identical (`make test` 18916 PASS). All 6 closure-cluster files plus
~31 io/misc/native-ctor files go green ON; no new ON regressions. Pin:
`t/gate-b-closure-capture-cluster.t` (OFF + ON both pass, matches raku).
**ON-survey delta: 61 → 24.**

## §1.4 flip blast-radius measurement (2026-07-02, debug + release `prove t/`)

A naive flip was implemented and measured, then reverted (branch
`refactor/lexical-scope-1.4`, no code landed). The flip:

- `declare_local`: allocate a **fresh** slot for a shadowing `my $x` (name already
  in an enclosing scope frame), same-scope redeclaration reuses, first declaration
  creates.
- `pop_local_scope`: restore `local_map[name]` to the outer slot (`Some(prev)`) or
  remove it (`None`) on scope exit.

Reads compiled correctly (each `GetLocal(slot)` uses the compile-time `local_map`),
and simple shadow tests (`my $x=1; { my $x=2; $x++; say $x } say $x` → `2 3 1`)
passed. But the full `t/` suite broke **57+ files through the `r`- prefix alone
(~40 % of the alphabet), i.e. an estimated ~120–140 total** — a catastrophic blast
radius. The failures cluster into exactly the three coupled mechanisms the campaign
predicted, confirming §1.4 is **not** a standalone slice:

1. **§1.3 env↔locals dual store (the dominant class).** `env` is a name-keyed
   `HashMap`; two live `$x` collapse to one env entry. `exec_get_local_op` /
   `exec_set_local_op` and the `BlockScope` save/restore all key off
   `code.locals[idx]` *by name*, so duplicate names cross-contaminate. Failing
   families: `*-writeback-coherence`, `*-captured-outer-coherence`,
   `closure-container-capture`, `cross-thread-shared-var`, `concurrent-cell`,
   `env-dirty-reconcile-coherence`, `lock-protect-*`, `element-*`, `pair-value-*`.
2. **§1.5 un-baked leaf writebacks (~80 `update_local_if_exists`/`find_local_slot`
   callers).** These resolve name→slot by `position` = the **outer** (first) slot,
   so an `undefine`/rw-arg/hyper/element writeback aimed at the live inner shadow
   hits the outer slot. Failing families: `method-rw-param-writeback`,
   `proto-rw-redispatch`, `nextsame-rw-redispatch`, `for-quanthash-values-rw`,
   `hyper-meta-assign-list`, `capture-element-writethrough`.
3. **Compile-time scope-exit resolution.** `pop_local_scope` removing a name from
   `local_map` changed how a post-block reference compiles (`block-lexical-scope.t`
   "chained our/my does not leak" no longer dies with X::Undeclared).

**Conclusion (decisive):** the flip cannot land as the default until (a) the
env-as-source-of-truth for locals is slot-indexed (§1.3) so duplicate names stop
colliding, and (b) every remaining leaf writeback carries a compile-time slot
(§1.5 S10+). Baking leaf sites one-at-a-time still leaves class 1 (env coherence)
fatal, so **§1.3 is the load-bearing prerequisite** — it must come before, or fused
with, the default flip. The per-variable opcode leaf slices (S1–S9) are done; the
remaining leaf sites are multi-var / runtime-derived and do not reduce class-1
breakage. See the ANALYSIS.md §1.4 investigation memo.

## §1.4 shadow-slot activation, gated (2026-07-03, branch `refactor/lexical-scope-1.4-shadow`)

Rather than revert-and-wait, the shadow-slot allocation now **lands behind an
env-var gate** (`MUTSU_SHADOW_SLOTS`), the same pattern the env_dirty campaign used
(`MUTSU_NO_BLANKET_RECONCILE`, …). This is the shadow-half of the campaign, run in
parallel with the §1.3/env substrate half.

- **`declare_local`** (gate on): a same-scope redeclaration reuses; a name bound in
  an enclosing scope becomes a genuine shadow with its **own fresh slot**; a first
  declaration allocates normally.
- **`pop_local_scope`** (gate on): restores the outer binding in `local_map` for
  every shadowed name on scope exit. `None` (first-declaration) entries are left in
  `local_map` (monotonic, matching the default build) so the runtime
  `block_declared_vars` machinery keeps enforcing out-of-scope errors — this avoids
  the naive flip's class-3 (`block-lexical-scope.t`) breakage.
- **Gate off (default / CI):** both functions take an early return that is
  byte-identical to the pre-campaign code (`alloc_local` get-or-create; pop is a
  no-op). CI stays green.

`MUTSU_SHADOW_SLOTS=1` toggle-ON baseline (2026-07-03, debug `prove t/`): shadowing
is correct for simple cases (`2 3 1 30 20 10` on nested `my $x`/`my $y`), but the
suite still shows the class-1 + class-2 breakage above (closure-capture,
captured-outer-writeback, cross-thread, quanthash-rw, …). That failure set is the
campaign's burndown target: the §1.3 half drives class-1 to zero (slot-indexed env)
and the §1.5 S10+ leaf baking drives class-2 to zero, after which the gate default
is flipped and the `exec_block_scope_op` whole-`locals` clone is removed.

**Task split:** this branch owns the shadow-side compiler machinery (declare/pop);
the env↔locals slot-indexing (§1.3) is the sibling half. Keep the gate OFF-default
until both halves make the toggle-ON survey green.

## §1.4 toggle-ON roast survey (2026-07-03, debug, full whitelist 1345)

`MUTSU_SHADOW_SLOTS=1 MUTSU_BIN=target/debug/mutsu prove -e scripts/run-roast-test.sh
$(cat roast-whitelist.txt)` → **102 files fail**; re-running only those 102 with the
gate OFF, **19 also fail** (flaky / pre-existing) leaving **83 genuine toggle-ON
regressions**. Every regression involves a variable actually shadowing an enclosing
same-name binding (non-shadowed `my $x` is byte-identical even ON, since
`declare_local` first-declaration → `alloc_local` and `pop` leaves `None` entries).
By synopsis: S32 16, S17 14, S02 14, S04 12, S03 10, S06 5, S09 4, misc 8. Root-cause
classes and the burndown owner:

- **class-2 leaf writeback (this campaign, bakeable — prefer the compile-time slot):**
  - `undefine($scalar)` → the rewrite hand-emitted `AssignExpr(name)`; **fixed** by
    `emit_assign_local_or_name` preferring `AssignExprLocal(slot)` like the general
    assign path (`roast/S32-scalar/defined.t` #31 now green ON). *(#4085)*
  - list assignment / parenthesized-`my` / lvalue-chain stores
    (`($a,$b)=…`, `(@a,%h)=…`, `(my $a)=…`, `($c=3)=4`) — the four
    `__mutsu_assign_callable_lvalue` handlers + the per-target stores in
    `compile_expr_call` hand-emitted `AssignExpr(name)`; **fixed** by routing all
    through `emit_assign_local_or_name` (`roast/S03-operators/assign.t` non-TODO ON
    failures 5→1, only the unrelated `//= … for` #291 remains). *(this branch)*
    - **`emit_assign_local_or_name` is GATED on `shadow_slots_active()`.** With the
      gate OFF it emits the original `AssignExpr(name)` verbatim (byte-identical),
      only preferring `AssignExprLocal(slot)` when shadows are live. Required
      because `AssignExprLocal` and `AssignExpr` are **not** interchangeable for
      `@`/`%` targets: the name-based op runs an extra attribute-cell mirror +
      container-identity path a circular `.raku.EVAL` roundtrip depends on
      (`roast/S32-array/perl.t` #7 regressed OFF when the bake was ungated —
      caught only by `make roast`, NOT `make test`). **Lesson: validate the OFF
      path with `make roast`, not just `prove t/`, before landing a leaf bake.**
  - `let`/`temp` restore (`S04-.../let.t`, `temp.t`): the scope-exit restore drains
    through the SHARED `apply_pending_rw_writeback` (`find_local_slot` by name) — the
    same drain rw-args use, so it belongs with the sibling (§1.3-adjacent) half, not a
    standalone list-assign-style bake.
  - rw-arg writeback (`S06-traits/is-rw.t`, `lvalue-subroutines.t`, `substr-rw.t`):
    `pending_rw_writeback_sources` by name.
  - hyper / metaop writeback (`S03-metaops/cross.t`, `zip.t`, `reverse.t`,
    `eager-hyper.t`): `write_back` target resolved by name.
  - for-loop rw / quanthash `.value`/`.kv` writeback (`S04-statements/for.t`,
    `S32-array/delete-adverb*.t`).
- **class-1 env↔locals dual store (§1.3, sibling half):** the name-keyed env cannot
  hold two live `$x`. Dominates S17 concurrency (shared-var writeback across threads:
  `S17-supply/*`, `S17-scheduler/*`, `Channel.t`, `lock.t`) and S02 aggregate
  writeback (`hash.t`, `set.t`, `baghash.t`, `capture.t`, `pair.t`). Not bakeable by
  a single compile-time slot; needs slot-indexed locals.

**Plan:** burn down class-2 leaves one slot-bake PR at a time (each behavior-
preserving with the gate off, verified by the toggle-ON roast file flipping green);
class-1 is unblocked only by the §1.3 half. Flip the gate default and drop the
`exec_block_scope_op` `locals.clone()` once the toggle-ON whitelist survey is green.

### Root-cause fix: shadow ⟺ active-ancestor, not `local_map` presence (2026-07-03)

The toggle-ON `prove t/` burndown went from **~20 failing files → 1** with a single
compiler fix. `declare_local` classified any name already in `local_map` as a
shadow — but `local_map` retains names from already-popped **sibling** blocks (kept
monotonic so the out-of-scope machinery works). So a sibling `my $a` was minted a
spurious fresh slot, creating a duplicate `code.locals` entry that corrupted every
by-name (`position`/`rposition`) writeback resolver (`\($a)` write-through, rw-arg,
capture-element, per-iteration closure capture, …) — they read the WRONG `"a"` slot.

Fix: a genuine shadow requires the name to be declared in an **active enclosing
(ancestor) scope frame** still on the `local_scopes` stack — not mere presence in
`local_map`. A leaked-sibling name takes `alloc_local` (reuses the sibling's slot,
no duplicate). This greened the class-1 closure-capture families AND the class-2
writeback families at once (they were all the same spurious-duplicate root cause).

Remaining toggle-ON `t/` failure (1): `lexical-scope-slot-writeback.t` test 3 —
`undefine($foo)` inside a genuine nested shadow writes back via
`apply_pending_rw_writeback` → `find_local_slot` (position = outer), not the live
inner slot. `s///` (S1 baked `lhs_slot`) and `++` (S2/S3 baked slot) already hit the
inner slot; `s///` (S1 baked `lhs_slot`) and `++` (S2/S3 baked slot) already hit
the inner slot. The parallel `emit_undefine_scalar_store` fix above
(`AssignExprLocal(slot)`) closes this last `t/` failure too — so the toggle-ON
`prove t/` survey is now fully green. (All gated; the gate OFF is byte-identical
and CI-green.)

**Impact on the roast survey:** the ~102-file / 83-regression roast count above was
measured with the OLD `local_map`-presence shadow rule, which minted spurious
sibling-shadow duplicates. This active-ancestor rule removes that whole spurious
class, so the real toggle-ON roast regression set is expected to be far smaller —
a fresh survey should be re-run on top of this fix before burning down class-2.

## §1.4 toggle-ON roast survey #2 (2026-07-03, after #4085-4088, debug)

Fresh full-whitelist survey on top of the active-ancestor fix (#4087), temp/let
bake (#4088), undefine (#4085) and list-assign (#4086): **24 files fail ON; 14 also
fail OFF (flaky — S17 concurrency, `state.t`, `catch.t`, `private.t`, atomic/cas,
promise/stress), leaving 10 genuine toggle-ON regressions:**

`6.d/S32-str/sprintf-{d,x}.t` (now TODO-only), `S02-types/hash.t`,
`S02-types/whatever.t`, `S06-advanced/wrap.t`, `S09-typed-arrays/native-str.t`,
`S13-overloading/metaoperators.t`, `S17-supply/Channel.t`, `S32-array/perl.t`,
`S32-list/reverse.t`.

**The clean, single-site dedicated leaves are exhausted** (undefine, list-assign
done). Each remaining genuine regression is one of:

- **element/index-assign on a shadowed target** — the highest-value NEXT leaf.
  `$a<x> = 42` / `$a<a b c> »=» 42` on a `my $a` shadowing an outer `$a`
  (`metaoperators.t` #14-16; a single-key `»=»` collapses to `IndexAssign`) writes
  the auto-vivified hash back to the OUTER slot. Path: `IndexAssignExprNamed`
  (opcode carries a compile-time `name_idx`) → `exec_index_assign_expr_named_op`
  (`vm_var_assign_index_named.rs`, ~13 `find_local_slot(code, &var_name)` /
  `update_local_if_exists(code, &var_name, …)` target-var sites). **Plan:** bake a
  `target_slot: Option<u32>` onto `IndexAssignExprNamed` (**6 emit sites**:
  `expr_method.rs` ×2, `expr_closure.rs` ×2, `expr_ops.rs`, `expr_call.rs`), thread
  it through the 13 *target-var* resolvers via `resolve_local_slot` /
  `write_local_slot_or_name`. **Alias nuance:** `var_name` is redirected to a
  `__mutsu_sigilless_alias::` target near the top of the exec — compute
  `eff_slot = if aliased { None } else { target_slot }` once and pass it everywhere,
  so an aliased target falls back to by-name (the alias is a *different* variable).
  Leave the `source_name`/`src` `:=`-cell sites by-name (distinct variable, future
  slice). Byte-identical OFF (`local_map.get == position` with no duplicates; same
  slot index, not a different op — unlike the #4086 `AssignExprLocal` divergence),
  so no gate strictly needed — but **validate with `make roast`, not just
  `prove t/`** (the perl.t lesson). Medium-large: 6 emit sites + opcode + 13 exec
  sites — a focused single-slice PR of its own.
- **hyper `»=»` multi-key** (`metaoperators.t`): genuine `HyperMethodCall` path →
  `write_back_hyper_target_var` (`find_local_slot`/`locals_set_by_name` by name);
  needs a baked `target_slot` on the hyper opcode. Same shape as above.
- **`my @a := EVAL "my $t @"`** (`native-str.t` #8): `:=` bind + EVAL carrier —
  ANALYSIS §1.4 addendum case (3), genuinely hard (EVAL is a separate scope).
- **class-1 / §1.3 (roast:0's sibling half):** `hash.t`, `Channel.t`, `wrap.t`
  (closure captures the wrong-slot lexical), `reverse.t` (`@a`/`$b`/`.=` aggregate
  coherence) — the name-keyed env can't hold two live bindings.
- **scope promotion** (`whatever.t` #45 "did not get promoted into its own scope") —
  a declaration-model/scope question, not a writeback leaf.
- **`perl.t` #7 under ON** — the list-assign `@`/`%` `AssignExprLocal` circular
  `.raku.EVAL` divergence (gated OFF in #4086; the ON path still needs the
  attribute-cell-mirror parity before the default flip).

**Next concrete slice: bake `IndexAssignExprNamed.target_slot`** (element/index-assign
on a shadowed target). It is the cleanest of the remaining dedicated-site leaves and
does not touch the shared `apply_pending_rw_writeback` drain (roast:0's half).

## §1.4 toggle-ON survey #3 (2026-07-03, after #4091 rw-arg + IndexAssignExprNamed on main)

Re-probed the survey #2 regression set on top of the rw-arg writeback drain bake
(#4091, roast:0's `apply_pending_rw_writeback` slot half) and the
`IndexAssignExprNamed.target_slot` bake now on `main` (roast:1's element-assign half).
**The class-2 leaf regressions from survey #2 are gone:** `S03-metaops/cross.t`,
`zip.t`, `S02-types/pair.t`, `S13-overloading/metaoperators.t` (#14-16 `»=»`), and
`S17-supply/Channel.t` all pass **ON** now. `for.t`/`pair.t` residual failures are
pre-existing (identical OFF), not shadow regressions.

**Remaining genuine toggle-ON regressions are ALL class-1 §1.3** (the name-keyed env
cannot hold two live same-name bindings; reads use `GetLocal(slot)` and every
bakeable leaf writeback now carries a compile-time slot, so what's left is purely
the env-coherence half):

- `S32-list/reverse.t` #9-10 — `my @a="foo"; my @b=@a.reverse; my $b=@a.reverse;`
  where `@a`/`@b`/`$b` shadow earlier same-name blocks. `$b` (scalar) / `@a` aggregate
  coherence collapses because env keys `a`/`b` hold the wrong live binding.
- `S02-types/hash.t` #80-81 — `$/`/`$0` as hash keys inside a `my %h` that shadows an
  earlier `%h`; the smartmatch `$/` env sync collides with the shadowed `%h` slot.
- `S06-advanced/wrap.t` (+2) — a closure captures the wrong-slot lexical (env capture
  reads the outer name).
- `S02-types/whatever.t` #45 — "did not get promoted into its own scope": a
  declaration-model / scope-promotion question, not a writeback leaf.

**Conclusion: §1.4 class-2 leaves are exhausted** (all bakeable single-site writebacks
done; `substr-rw`/scalar-`given`-topic remain but are niche AND route through special
lvalue/topic paths — see memory `leaf5-given-topic-rabbithole`). The gate default flip
is now blocked ONLY on §1.3 (slot-indexed locals / eliminate the env-by-name mirror
for locals) + removing the `exec_block_scope_op` whole-`locals` clone.

## §1.3 plan (slot-indexed locals — the remaining load-bearing refactor)

The class-1 root: `env` is a name-keyed `HashMap` that mirrors every local by name, so
two live `$x` (outer + inner shadow) collapse to one `env["x"]`. Reads are already
slot-indexed (`GetLocal`), and the shadow-slot machinery gives each shadow its own
`locals` slot; the break is only the paths that still round-trip a local **through env
by name**: closure capture (`box_captured_lexicals` / `closure_env_overrides`), regex
interpolation sync (`sync_regex_interpolation_env_from_locals`), cross-thread env copy,
and the `.=`/aggregate writeback env mirror.

Concrete first slices (each behavior-preserving with the gate OFF; roast is the net):
1. **Audit the env-mirror-for-locals write sites** (`set_env_with_main_alias` /
   `env_mut().insert` for names that are also `code.locals`). Enumerate which are
   load-bearing for closures/regex/threads vs. vestigial.
2. **Make closure capture slot-addressed** for shadowed locals: capture the `locals`
   slot (or its `ContainerRef` cell), not `env[name]`, so a wrapper/closure over an
   inner shadow reads the inner cell (fixes `wrap.t`). This overlaps ADR-0001 layer-3a
   (container cells) — coordinate with GC, do NOT box scalars eagerly.
   **DONE — see S13 in the slice checklist** (emit-point capture slots + the
   dup-named shadow boxing trigger; boxing stays scoped to captured-and-mutated
   scalars, no eager scalar boxing).
3. **Slot-key the regex-interp / smartmatch env sync** so `$/`/`$0` and a shadowed
   `%h` don't collide (fixes `hash.t`).
4. **Remove the `exec_block_scope_op` `locals.clone()`** once 1-3 hold, then flip the
   `MUTSU_SHADOW_SLOTS` default and burn down the resulting roast set.

**ADR note:** §1.3 slot-indexed locals is a large architectural call and its closure
half overlaps ADR-0001 (Track B container cells fused with GC). Per CLAUDE.md, land it
as coordinated slices with roast as the safety net, and do NOT eagerly box scalar
locals (the flaky-capture trap, ANALYSIS §1.3). A dedicated session per slice.
