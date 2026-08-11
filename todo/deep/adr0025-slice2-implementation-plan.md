# ADR-0025 slice 2 — concrete implementation plan (decl-site cells for vouch-refused captured scalars + cross-thread rider)

Companion to `docs/adr/0025-captured-scalar-cells-value-kind-blind.md`
("Slice 2 (planned)"). This file turns the ADR's mechanism paragraph into a
step-by-step, Sonnet-implementable plan, updated with the 2026-08-11
diagnosis evidence (see the parent ticket
`todo/deep/closure-read-only-capture-loses-to-caller-env-same-name.md`,
item 1).

## Ordering change vs the ADR text: do the rider FIRST

The ADR listed the "cross-thread audit rider" as a companion to the
decl-site set extension. The response-serializer evidence inverts the
priority: in the real failing file the cell ALREADY forms (slice 1's
trigger fires — `$encoder` is creator-reassigned, the check closures are
escaping array-literal elements) and mainline write-through works; what
fails is the WORKER-side chain, where a stale plain `encoder` entry beats
or replaces the cell during cross-thread dispatch. So:

### Step 0 (independent, highest value): find and fix the stale-plain-over-cell lane

1. Repro: `tmp/h2rs-probe.raku` run via
   `bash -c 'INC=$(cat tmp/cro-work/inc-paths.txt); target/debug/mutsu $INC tmp/h2rs-probe.raku'`
   — deterministic. The `$pre`/`$probe` closures print `Encoder|<new>` from
   mainline and `Encoder|<old>` from the tap thread.
2. Use `rust-gdb -batch` (per CLAUDE.md; do NOT eprintln-rebuild): break on
   the closure-dispatch captured-env merge (`vm_closure_dispatch.rs`,
   `entry_or_insert_sym` / the overwrite branch for `ContainerRef` and
   `authoritative_free_vars`), and on `sync_shared_vars_to_env` and
   `set_env_with_main_alias_sym`, conditioned on the `encoder` key, in the
   tap-dispatch thread. The question is purely control-flow ("who installs
   the plain value / who skips the cell"), which gdb answers without
   decoding NaN-boxed values.
3. Candidate sites (ADR-0024 implementation notes points 5-7): the two
   sites fixed via the mainline-map-specific `mainline_lexical_cell` lookup
   (`assign_rw_target_expr`, `set_env_with_main_alias_sym`) must preserve
   ANY `ContainerRef` env entry, not just mainline-map hits; point 6's fix
   is already generic and is the model. Also audit
   `sync_shared_vars_to_env`'s overlay direction: a plain store entry must
   never overwrite a `ContainerRef` env entry for the same name (deref and
   write INTO the cell, or skip).
4. Acceptance: `http2-response-serializer.rakutest` passes fully (test 18
   is its only remaining failure); pin a `t/` test if a Cro-free repro
   falls out of the gdb session (the current Cro-free reduction does NOT
   reproduce — the extra ingredient is in Cro's transformer internals, so
   the pin may have to wait for the mechanism to be named).

### Step 1: compiler — the `cell_captured_ref_slots` set

Location: `compute_free_vars` in `src/opcode.rs` (~line 5085 already
computes `own_container_writes` and `own_call_arg_sources`; the vouch set
`authoritative_free_vars` is baked nearby).

- New `CompiledCode` field `cell_captured_ref_slots: Vec<u32>` (slot
  indices, mirroring `needs_cell_named_sub_ref_slots`; keep
  `size_of::<OpCode>() <= 48` — this is a `CompiledCode` field, not an
  OpCode payload, so the guard is unaffected, but check `opcode_size_guard`
  anyway).
- Contents: slots of plain-scalar OWN locals that are (a) captured by any
  nested closure, and (b) NOT in `authoritative_free_vars`'s vouch set —
  computed as `captured_mutated_locals ∪ (own_container_writes ∩ captured)
  ∪ (own_call_arg_sources ∩ captured ∖ scalar_bind_locals)` per the ADR
  (this is the exact complement of the authoritative set within the
  captured set, so the dichotomy is exhaustive by construction).
- Exclusions stay as in slice 1: `type_constrained_unboxable` (cas,
  S17-lowlevel/cas.t), Package/Array/Hash/Sub/Proxy VALUE kinds are no
  longer relevant at decl time (the seed at declaration is Any/initializer
  result), but the `@`/`%`/`&` SIGIL lanes and Proxy remain out of scope.

### Step 2: VM — box at declaration

- Extend the existing consumption path: `vm_var_assign_set_local.rs`
  (~lines 245-268) gates declaration-time boxing on
  `needs_cell_named_sub_ref_slots`; add `cell_captured_ref_slots` to the
  same gates so `box_decl_local_cell`
  (`vm_var_assign_local_get.rs:316`) fires for these slots at their
  declaration site.
- `box_captured_lexicals` (`vm_register_ops.rs:819`) stays as the
  no-op-when-already-cell backstop.
- Watch the `self_capture_decl_locals` interaction: the "clear the stale
  ContainerRef on redeclaration" step must keep its existing skip rules
  (see the field docs in `opcode.rs`), and a loop redeclaration must still
  get a FRESH cell per iteration (ADR-0023 fresh-binding provenance; pin
  `t/for-loop-param-start-sibling-isolation.t`).

### Step 3: perf gates (mandatory, in order — ADR-0025)

1. Add a `MUTSU_VM_STATS` counter `decl_cell_boxes` (pattern:
   `record_spawn_seeding` in `src/vm/vm_stats.rs`). Assert ≈0 across
   `benchmarks/` in a DEBUG build (counters are optimization-independent
   per CLAUDE.md).
2. `roast/S32-num/int.t` wall-clock in a RELEASE build before pushing —
   the #2749 blowup (~1s → 150s) is the named canary.
3. Bench CI history (`git show origin/bench-data:bench-history.tsv`) is
   the final verdict after merge.
4. If the canary regresses: diagnose the cost source (per-iteration
   redeclaration re-boxing vs env insert) — hoist boxing out of
   per-iteration redeclaration or intern the cell in the declaration plan.
   Do NOT re-add an escape/value-kind gate (correctness must not be
   bounded by an incomplete analysis; CLAUDE.md gain/risk).

### Step 4: tests / acceptance

- New `t/` pin for the call-arg-stored closure shape
  (`@registry.push($cb)` / `.tap($cb)` / ctor named-arg) reading a
  creator-mutated scalar after rebind — the shape slice 2's set newly
  covers.
- The ADR's listed pins stay green: `t/closure-capture-instance-cell.t`,
  `t/for-loop-param-start-sibling-isolation.t`,
  `t/named-sub-lexical-scope.t`, the merge-site liveness example
  (`my $s = 0; @cb.push({ $s }); $s = 42` must read 42).
- End state (with step 0): `http2-response-serializer.rakutest` fully
  green; re-run the loop-param repro from the (now-resolved) loop-param
  ticket per the ADR's co-requisite note; Cro session files re-check waits
  on `todo/tickets/http-session-tests-crash-rc139-on-main.md`.

## What slice 2 does NOT cover (unchanged)

- `http2-request-parser.rakutest` test 49 — aggregate (`%`) clobber via
  nested-whenever registration, its own deep ticket
  (`nested-whenever-registration-clobbers-sibling-event-aggregate-writes.md`).
- Type/`where`-constrained scalars, `$`-held Array/Hash, `@`/`%`/`&`
  rebinding staleness — ADR-0025 slice 3.
