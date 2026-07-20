# Inventory: `gc_contents_mut` aliased-write call sites

*(Step 1 of PLAN §2.1 — GC soundness tail. Generated 2026-07-20 by a close read of every
production call site. Companion to ANALYSIS §2.1 and ADR-0001.)*

`gc::gc_contents_mut(&Gc<T>) -> &mut T` (def: `src/gc/gc_ptr.rs:660`) performs a deliberate
aliased in-place mutation through `Gc::as_ptr(gc) as *mut T`, used to preserve Raku
**container identity** (a mutation through one alias is visible through every holder of the
same `Gc` node). It is a provenance violation (Rust UB by the letter of Stacked Borrows) plus
a genuine data race when the node is actually shared across threads.

Safe alternatives already in the tree:
- `Gc::make_mut(&mut self)` — COW: clones when strong-count > 1, so it does **not** preserve
  identity across aliases.
- `Gc::get_mut(&mut self)` — `Some` only if uniquely owned.

## Classification legend

- **(a) provably-unique** — the `Gc` is freshly owned here / not yet shared → a debug
  `assert!(strong_count == 1)` documents it; safe as-is.
- **(b) make_mut-COW-coverable** — identity NOT required → `Gc::make_mut` is semantically
  equivalent; convertible with the existing mechanism, no new type.
- **(c) needs-cell** — identity IS required AND the node can be genuinely shared → cannot use
  `make_mut` without breaking identity; needs a first-class element cell / `CellValue`
  (interior mutability).
- **(?)** unsure.

## Headline finding

The production surface is almost entirely bucket **(c)**. **51 of 54** sites are
identity-required shared writes; only **3** are bucket (a) (cyclic-structure construction on a
just-created node). There are **zero (b)** and **zero (?)** sites. Every guarded site literally
falls back to `make_mut` on the unique branch and reserves `gc_contents_mut` for the
shared+identity case — which is the definition of (c).

**Consequence:** `make_mut`-COW can retire **none** of the sites. The sound fix for the ~51
(c) sites is a first-class element cell / `CellValue` (interior mutability inside the
container), exactly the Track B / GC layer-3a fusion in ADR-0001. This is an architectural
campaign, not a cleanup sweep. The only cheap wins are documenting the 3 (a) sites and
hardening the buffered-clone uniqueness invariant (PLAN §2.1 Step 4).

## Summary tally

| Bucket | Count | Sites |
|---|---|---|
| (a) provably-unique | 3 | `vm_var_assign_ops.rs:375, 463, 516` |
| (b) make_mut-COW-coverable | 0 | — |
| (c) needs-cell | 51 | all other production sites |
| (?) unsure | 0 | — |
| **Total production** | **54** | across 20 files |

**Only mixed file:** `vm/vm_var_assign_ops.rs` (3× (a) + 2× (c)). Every other file is pure (c).

---

## Per-file detail

### `src/value/value_methods_a.rs` — pure (c)

All operate on a `Value::Hash` from `self.view()` (a variable's/container's own, possibly
aliased, hash); each is a documented container-identity autoviv/insert that must propagate.

- `:279` — `store_through_cell` — insert `ContainerRef` at a `HashEntryRef` terminal — (c).
- `:362` — `hash_autovivify` — insert empty Hash on missing key — (c).
- `:395` — `hash_autovivify_cell` — promote scalar leaf to cell / autoviv — (c).
- `:440` — `hash_slot_ref` — promote element to `ContainerRef` cell for `:=` — (c).
- `:479` — `hash_assign_at` — `hash_insert_through` on shared hash — (c).
- `:542` — `hash_entry_terminal` — walk-create intermediate hashes — (c).

### `src/value/value_methods_b.rs` — pure (c)

- `:10` — `hash_entry_write` — insert at terminal key — (c).
- `:24` — `array_push_in_place` — `items.push` (the canonical shared-push) — (c).
- `:45` — `ensure_array_child` — grow + vivify intermediate array element — (c).
- `:99` — `array_slot_ref` — grow + promote element to cell — (c).

### `src/value/view.rs` — pure (c)

- `:605` — `with_array_inplace` — run `f` on shared `ArrayData`; the documented §3
  container-identity chokepoint (explicitly rejects `make_mut`) — (c).

### `src/vm/vm_var_assign_ops.rs` — MIXED (3× a + 2× c)

- `:375` — `fixup_circular_hash` — self-referential hash cycle on freshly-created
  `result_arc` (count == 1; raw ptr only because self-cycle clones the same `Gc` while a
  `&mut` is live → borrow conflict, but node is provably unique) — **(a)**.
- `:463` — `replace_array_refs_in_value` — self-ref insert on freshly-created `new_hash_arc` — **(a)**.
- `:516` — `fixup_circular_array_refs` — self-ref cycle on freshly-created `result_arc` — **(a)**.
- `:554` — `array_inplace_reassign` — `*old_gc = new_data` through the original backing (shared) — (c).
- `:631` — `hash_inplace_reassign` — hash twin of the above — (c).

### `src/vm/vm_var_assign_index_named.rs` — pure (c)

- `:652` — scalar-list element write (mutable Scalar element inside a shared list) — (c).
- `:1661` — hash key write, guarded `strong_count > 1` (`make_mut` on unique branch) — (c).
- `:1759` — array element write, guarded `strong_count > 1` — (c).
- `:2520` — nested `%h<x>{...}` outer-hash vivify/write (make_mut would relocate `.WHICH` metadata) — (c).
- `:3092` — named hash key write (interior mutation, installs bind cell) — (c).
- `:3110` — named array element write (interior mutation) — (c).

### `src/vm/vm_var_assign_coerce.rs` — pure (c)

- `:109` / `:122` / `:135` — `quanthash_store_preserving_identity` (Set / Bag / Mix): `*old =
  data` into the variable's EXISTING QuantHash node (`old` from `env().get`), preserving node
  identity for every holder (env mirror, `:=` binds, captures) — (c).

### `src/vm/vm_var_assign_computed_attr.rs` — pure (c)

- `:28` — `assign_into_computed_target` (Hash) — insert on resolved inner hash — (c).
- `:35` — `assign_into_computed_target` (Array) — element assign on resolved inner array — (c).
- `:74` — `materialize_bound_slot_to_cell` — install cell at token terminal — (c).

### `src/vm/vm_var_assign_post_incdec.rs` — pure (c)

- `:728` — hash `$h<k>++`, guarded `strong_count > 1` — (c).
- `:751` — array `@a[i]++`, guarded `strong_count > 1` — (c).

### `src/vm/vm_var_elem_mutate.rs` — pure (c)

- `:150` — `tag_element_value_type_in_place` (Array) — set `value_type`/`declared_type` metadata — (c).
- `:161` — same for Hash — (c).

### `src/vm/vm_var_index_tracking.rs` — pure (c)

- `:141` — record assigned index into embedded `initialized` set, guarded `strong_count > 1`
  (pinned by `t/array-push-byref-coherence`) — (c).

### `src/vm/vm_misc_assign.rs` — pure (c)

- `:378` — name-based `HashEntryRef` materialization — install cell at token terminal — (c).

### `src/vm/vm_exec_dispatch.rs` — pure (c)

- `:1328` — SetGlobal `HashEntryRef` materialization — (c).
- `:3294` / `:3298` — `UndefineAggregate` clear on env array / hash — (c).
- `:3312` / `:3316` — same clear on locals-slot array / hash — (c).

### `src/vm/vm_call_method_mut_ops.rs` — pure (c)

- `:2054` — native `push`/`pop`/`shift`/`unshift`/`append` on env-bound array
  (`env_root_descended_mut`) — (c).
- `:2369` — native `splice` on env-bound array — (c).

### `src/vm/vm_call_method_ops.rs` — pure (c)

- `:1515` — `.shift`/`.pop` on a by-value receiver (`invocant = target.clone()` shares inner Gc) — (c).

### `src/vm/vm_hyper_method_ops.rs` — pure (c)

- `:635` — hyper write-back to non-variable target (`existing.items = items.clone()`) — (c).
- `:1008` — twin site in `exec_hyper_method_call_op` — (c).

### `src/runtime/methods_mut_dispatch.rs` — pure (c)

All guarded by `strong_count > 1`, falling back to `make_mut` when unique.

- `:1432` — `push`/`append` extend on env-bound array — (c).
- `:1498` — `pop` — (c).
- `:1539` — `unshift` insert — (c).
- `:1573` — `prepend` insert — (c).
- `:1624` — `shift` remove — (c).

### `src/runtime/methods_call_dispatch.rs` — pure (c)

- `:1002` — `%h.push(pairs)`, guarded `strong_count > 1` — (c).

### `src/runtime/methods_mut_method_lvalue.rs` — pure (c)

- `:350` — `$pair.value = X` where value is a live `HashEntryRef` (writes through to
  `%h{$p.key}` on the shared hash) — (c).

### `src/runtime/builtins_collection_deepmap.rs` — pure (c)

- `:274` — deepmap leaf write-back into source array — (c).
- `:358` — same into source hash — (c).

### `src/runtime/utils/shaped.rs` — pure (c)

- `:121` — `mark_shaped_array_items` — set `shape` metadata (shared by every holder;
  replaces a pointer-keyed side table) — (c).

---

## Infra (`src/gc/` + the `aliased_mut` wrapper — not classified)

The primitive/definition/re-export layer, not production mutation sites:

- `src/gc/gc_ptr.rs:660` — the `gc_contents_mut` **definition** (the single `Gc::as_ptr as
  *mut` cast). Doc/SAFETY refs at 315, 328, 344, 349, 396; unit test at 1086–1092.
- `src/gc/mod.rs:34` — re-export.
- `src/gc/safepoint.rs:5` — doc comment (re-entry boundary rule).
- `src/value/aliased_mut.rs` — the audited wrapper module: `arc_contents_mut` (line 69,
  dead-code Arc analogue), a shadow `gc_contents_mut` (line 84), and `gc_data_mut` (line 107,
  branches `strong_count > 1 → gc_contents_mut` else `make_mut`). This module *is* the intended
  single choke point; its module docs (1–35) already document the unsoundness and name Track B
  / first-class container cells as the real fix.
- `src/value/mod.rs:317` — `pub(crate) use crate::gc::gc_contents_mut;` re-export.

---

## What Step 1 tells the plan (PLAN §2.1)

1. **`make_mut`-COW is a dead end (Step 2 is empty).** No production site is convertible;
   every guarded site already uses `make_mut` for the unique case.
2. **The 3 (a) sites — DONE (2026-07-20)**: `debug_assert_eq!(strong_count(), 1)` added before
   each aliased `&mut` in `vm_var_assign_ops.rs` to document provable uniqueness. Zero behavior
   change (debug-only); the truly-unaudited surface is now the 51 (c) core.
3. **The 51 (c) sites require the `CellValue` / interior-mutability campaign** — a first-class
   element cell (`UnsafeCell`/lock inside the container) so identity-preserving in-place
   mutation is sound without a raw-pointer cast. This is fused with ADR-0001 (do not touch the
   sites twice) and is a large, high-blast-radius campaign, not a slice. **DEFERRED** (user
   decision 2026-07-20): a large architectural commitment for a soundness-only payoff, while 3a
   already closed the GC leak. **The mechanism framing is drafted in
   [ADR-0013](adr/0013-container-interior-mutability-cellvalue.md) (Proposed)**: a `GcCell` =
   `UnsafeCell` + debug/verify borrow-flag newtype (reads stay `&T` via `Deref`, zero read-path cost;
   Miri is the acceptance gate), rejecting the whole-container lock and deferring the narrow
   cross-thread race to ADR-0001 layer 3c. Flip to Accepted on greenlight.
4. **Buffered-clone invariant hardening — DONE (2026-07-20)**: `Gc::verify_unique_for_aliased_mut`
   machine-checks the `strong == 1 ⟹ unique` argument at `make_mut` / `get_mut` and the three (a)
   sites by asserting the backing `Arc` strong count equals the GC-visible `header.strong` (they move
   in lockstep for every live handle; the candidate buffer holds only `Weak`). Verify-gated
   (`MUTSU_GC_VERIFY=1`), compiled out in release, and skipped when `collecting()` /
   `other_mutators_active()` so it is a hard signal only under the single-threaded gc-stress CI and
   cannot false-positive on a transient collector clone in a multithreaded verify run. Reports via the
   collector's non-fatal `VERIFY FAIL` stderr line (PLAN §2.1 Step 4).
