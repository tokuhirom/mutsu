# First-class containers (container identity) implementation ledger

Implementation log for PLAN.md "🟣 Priority 2: Migration to first-class containers".
Like `docs/vm-dual-store.md` / `docs/vm-decoupling.md`, this is a ledger that records **an accurate map of the current state →
staged slices → per-PR progress**. Before touching any code, we pin down "what to fix where",
and avoid the "deref everywhere" regression that the past ContainerRef prototype ran into.

Final goal: the **fastest and most maintainable** Raku interpreter. Container unification is
a correctness fix and, at the same time, the maintainability win of deleting scattered workarounds.

> **★ Convergence (2026-06-18)**: This campaign **converges onto the same wall** as the dual-store unification
> ([docs/vm-single-store.md](vm-single-store.md)) final Slice F (removal of `env_dirty` = making `locals` the single authority). Slice F requires
> "env and locals must not diverge on containers", and that is exactly the guarantee that Phase 2/3 of this campaign builds. The ContainerRef cell
> already solves survival across COW within 1 store, but **the dual-store env↔locals divergence remains as a separate layer** (the fact that the pairs/slip
> carrier-drop breaks `element-bind-cell.t` even with the cell mechanism in place is the evidence). Both campaigns unify into one at Slice F.
> **The design for resolving this env↔locals divergence = [docs/env-locals-coherence.md](env-locals-coherence.md)** (recommended = make outer containers
> `ContainerRef` cells in env too, so both stores share the same cell = same shape as Phase 3 instance attrs, escape-aware). Slice E
> (closure upvalues, #3245/#3247) completed the single-store independence prerequisite. Only this coherence work remains.

---

## 1. Current state: containers/itemization are **fragmented into 3 representations**

mutsu does not have "Raku's container identity"; instead, 3 overlapping representations coexist.
This fragmentation is the substance of the debt, and the target of unification.

| # | Representation | Definition | Role | Main construction sites |
|---|------|------|------|------------|
| 1 | `Value::Scalar(Box<Value>)` | `src/value/mod.rs:1075` | itemization wrapper (`$(...)`) | `vm.rs:1814`(WrapScalar), `vm.rs:1803`(Seq itemize), `runtime/methods.rs:215/2327` |
| 2 | `Value::ContainerRef(Arc<Mutex<Value>>)` | `src/value/mod.rs:1078` | inter-variable shared cell for `:=` | `into_container_ref()` (`value/mod.rs:2136`). Callers are **only 4 sites**: `vm.rs:1475`, `vm_register_ops.rs:271`, `vm_var_assign_ops.rs:4880` |
| 3 | `ArrayKind::ItemList` / `ItemArray` | `src/value/mod.rs:844-846` | itemization flag baked into the Array value | `ArrayKind::itemize()` (`value/mod.rs:867`), `OpCode::Itemize` (`vm.rs:1792`) |

The decont (de-containerize) helpers are also scattered:
- `Value::deref_container()` — `value/mod.rs:2112` (reads a ContainerRef)
- `Value::decontainerize(&self) -> &Value` — `value/mod.rs:2367` (strips Scalar)
- `ArrayKind::decontainerize()` — `value/mod.rs:878` (drops the item flag)

→ **There are 3 "strip the container" operations, and each caller decides per context which one to use.**
This is the breeding ground of "deref everywhere".

---

## 2. Canonical case study: a `:=`-bound list is not flattened (reduce.t 62)

```raku
my $l := (1,2,3);   # raku: $l is the List itself. .VAR is List
my @a = $l;         # raku: 3 elements (the List flattens)
say @a.elems;       # raku: 3 / mutsu: 1   ← bug
```

### Why this happens (trace)

1. `my $l := (1,2,3)` and `my $l = (1,2,3)` **both store `Value::Array(items, List)` (non-
   itemized)**. `:=` is distinguished in the compiler via `MarkBindContext`
   (`compiler/stmt.rs:768/795`), but the scalar-store path places the same bare List for bind/assign
   (`normalize_scalar_assignment_value` does not itemize, `vm_var_assign_ops.rs:766`).
   → **Container status is lost at the storage stage**. `$l.VAR.^name` is `Scalar` in both cases (raku says
   `List` for a binding).
2. When compiling `@a = $l`, `compile_assignment_rhs_for_target`
   (`compiler/stmt.rs:233`) **unconditionally emits `OpCode::Itemize` whenever the target is
   `@` and the RHS is `Expr::Var`** (`stmt.rs:245`).
3. `OpCode::Itemize` (`vm.rs:1792`) converts `Array(.., List)` → `Array(.., ItemList)`.
4. Array-assignment flattening respects the ArrayKind (`ItemList` → 1 element, `List` → flatten).
   In fact `@a = (1,2,3)` (a literal, not going through a Var) yields 3 elements and `@a = $(1,2,3)` yields 1 element —
   **these already work correctly**. The problem is (2): **a Var-mediated scalar read is unconditionally itemized**.

### Why it cannot be fixed locally

Even if we wanted to make the `Itemize` at `stmt.rs:245` "skip if the variable is bound", **the runtime values are
completely identical for bind and assign** (both are bare `Array(.., List)`). The compiler also does not know the boundness of a `$l` declared elsewhere.
→ The only way to distinguish them is to **carry container status at the storage level** = Phase 1.
Local hacks like separately tracking the set of bound variables are exactly the scattered workarounds the strategy warns against.

---

## 3. Design: consolidate decont into a single chokepoint

The Rakudo/MoarVM way. Containers exist **only at storage sites** (variable slots, array/hash elements, attributes),
and **value reads always decont through the single path of VM operand fetch**.
Value ops for arithmetic, comparison, dispatch, etc. pop "already deconted" values from the stack, so they are **unchanged**.
Containers are visible only on the few paths that explicitly request an lvalue/container:

- `:=` bind (container replacement)
- `is rw` / `is raw` parameters (aliasing the caller's container)
- `.VAR` / `=:=` (reflection on container identity)
- take-rw (preserving element container identity)
- itemization decision (1 element vs flatten in list context)

With this design, the consumer surface **inverts** from "hundreds of value ops" → "the handful above".

### Landing this in mutsu

1. **Establish a canonical `decont()`**: unify the current 3 decont helpers into 1
   (a single function that strips `Scalar`/`ContainerRef`/item-flag all at once). Make this the VM's
   value-read invariant.
2. **Value-stack invariant**: "values pushed onto the stack are always deconted". Only paths that need an lvalue/container
   fetch the raw cell via dedicated opcodes (`GetLocalContainer`/`IndexContainer` etc.).

---

## 4. Staged slices (an order that avoids big-bang regressions)

> **The canonical implementation order is PLAN.md §🟣 "Implementation order (architecture first)".** The north star is cleanest × fastest, and
> the progress metric is **"number of duplicated/special-case mechanisms deleted"** (not the roast pass count). The Phase breakdown below remains as
> a map, but work proceeds via the unified resolution "keystone = escape analysis" → "cell-ification of scalars" (do not add
> individual boxing patches).

### Current duplication (4 mechanisms coexist for closure capture alone; step 1 subsumes → delete)

There are 4 ad-hoc mechanisms for a closure to see its captured variables, leaving gaps (e.g. single escaping closures).
Escape analysis (PLAN.md implementation order step 1) unifies these into a single needs-cell decision, and step 2's cell-ification **deletes** them:

**⚠️ Confirmed by the 2026-06-08 experiments: the "delete in step 2" assumption in the table below is wrong. Only `multi_captured_mutated_locals`
was subsumed and deleted in #2758. The remaining 3 mechanisms are non-redundant and cannot be deleted (reasons on each row).** Consistent with PLAN.md §🟣 STATUS.

| Mechanism | Role | Deletable? (empirical) |
|------|------|----------------|
| `owned_captures` (`compute_owned_captures`) | Loop per-iteration **value freezing** | ❌ read-only captures are not cell-ified, no substitute |
| `closure_captured_state` (per-instance value freeze + writeback) | State maintenance for returned closures | ❌ load-bearing for precompiled deserialize (cells cannot be serialized, `precompilation.t` regression)|
| `box_captured_lexicals` | Cell creation at capture time (escape/loop) | ❌ capture time is the right place. Moving to declaration time breaks `let`/`temp` restore (`let.t` regression)|
| `multi_captured_mutated_locals` (#2751) | Sibling-closure boxing (interim proxy) | ✅ subsumed by the #2758 escape signal, deleted |

The env snapshot (`clone_env`/`flattened`) remains as the foundation of capture. **Cells subsume "runtime shared mutation" but do NOT subsume
declaration-time mechanisms (let/temp), serialization, or read-only value freezing**, so the top 3 mechanisms play distinct roles that *complement* cells.

### Step 2 retraction log (2026-06-08)

- **"Move box to declaration time and delete it" = attempted → reverted**: We worked out that captured locals in block/loop/sub have sigil-less names (`a`) =
  `simple=false` → slow path, and got it running, but it deterministically regressed `let`/`temp` restore (`S04-blocks-and-statements/let.t`).
  Declaration-time cell-ification runs before the `let`-save, so the save stores the cell Arc and restore becomes impossible. **Boxing at capture time is the right place.**
- **"Delete `closure_captured_state`" = attempted → closed (PR #2765)**: Hypothesis was that step 1 cell-ifies assigned/returned closures making it redundant.
  Disabled it → make test PASS, 166 state-sensitive files release 0-fail. But it regressed `S10-packages/precompilation.t`
  (GH2897): the closure returned by `gen-counter` is created at BEGIN/precompilation time and **serialized to disk**;
  ContainerRef cells are not preserved across processes, so after deserialize this side table carries per-call state = **load-bearing**.
- **Lessons**: (1) the boundary of cell-ification (runtime shared mutation only). (2) These release-only regressions pass `make test` — any change touching closure
  capture must **compare main vs branch in release** and always include `precompilation.t`. (3) Mechanism deletion for containers hits its ceiling here.
  The remaining correctness items are handled one-off in PLAN.md §🟣 "Opportunistic backlog".

### Phase 0 — decont chokepoint groundwork (behavior-preserving)
- [ ] Unify the 3 decont helpers into a single `decont()` (`value/mod.rs`). Replace existing call sites;
      behavior fully unchanged = verified by exact roast match.
- [ ] Inventory the value-read paths and establish the "push deconted values" invariant. Add lvalue-only opcodes
      (behavior-preserving since there are still almost no containers).
- Verification: `make test` + full roast exact match (zero diff is the pass criterion for Phase 0).

### Phase 1 — first-class containerization of scalars
- [ ] `$` variables hold a `Scalar` cell. `=` stores into the cell, `:=` swaps the binding (bare value),
      itemization is a cell wrap. Implement the storage-stage container status of `§2`.
- [ ] Change the unconditional `Itemize` at `stmt.rs:245` to **itemize only when the variable holds a container**
      (a bound variable's bare List flattens).
- Solves: reduce.t 62, `=:=`/`.VAR` (scalar), sibling-closure sharing (lever C completed),
  S02 variables-and-packages variable capture, S03-binding/scalars.

### Phase 2 — containerization of array/hash elements
- [ ] Make elements cells like COW `Arc<Vec<Scalar>>`. This is the hottest representation, so it comes after Phase 1.
- Solves: take-rw (gather.t 38), `@a[0] :=`, deep `>>++`/`deepmap(++*)` (hyper.t 330-333),
  object-hash, S12-methods/accessors, S12-attributes/instance, S03-binding/nested.

#### Investigation record 2026-06-11 (prototype experiment → staged order fixed. Code reverted)

User direction (2026-06-11): proceed with "cell-ify only bound elements (escape-aware)" — do not cell-ify all elements;
promote only elements bound with `:=` to `ContainerRef` cells, and decont reads at a single chokepoint.
A prototype was applied, **demonstrating both the correctness of the mechanism and the breadth of the write surface** (then reverted):

- **Current element binding is an ad-hoc back-reference**: `$x := @a[0]` places `Value::ArraySlotRef { array: Arc<Vec<Value>>,
  index }` (`value/mod.rs` `array_slot_ref`, emitted only on `:=` bind via the `IndexAutovivify` opcode) into
  `$x`. `HashSlotRef`/`DeferredHashAccess` are the same shape. **Shallow binds (`$x:=@a[0]`/2 levels) work**, but deep
  `$struct[1]<key><subkey>[1]` fails because **the back-reference goes stale** (nested.t 3/4/7/8/19/21/26/28/31-37/40-43):
  when a subsequent write re-resolves the path, if an intermediate container is cloned to a different Arc by COW, the SlotRef's captured Arc
  stays old = writes no longer reach the bound variable.
- **Why cells are the canonical solution (demonstrated)**: promoting an element to `ContainerRef(Arc<Mutex<Value>>)` means the
  **Arc<Mutex> is shared across clones even under COW clone**, so the leaf cell's identity survives across paths of arbitrary depth.
  Changing `array_slot_ref` to "replace the element in-place with a ContainerRef and return the same cell" makes the binding cell-shared.
- **Read chokepoint**: a single int read decontifies by adding `ContainerRef => cell.lock().clone()` to the `Some(value)=>value.clone()`
  in `resolve_array_entry` (`vm_var_ops.rs:146`) (**ordinary arrays have no ContainerRef elements, so this is a no-op**).
- **❌ Blocker = the write surface is wide ("deref/write-through everywhere")**: element writes for `@a[i] = v` are scattered across
  **a dozen-plus sites** (`vm_var_assign_ops.rs` 158/180/2511/2622/2692/3070… plus each deep/named/generic index-assign
  arm, `array_slot_write`). Each site needs "if the existing element is a ContainerRef, write through the cell; otherwise replace".
  Miss even 1 site and that path's write **replaces the leaf cell and severs the binding** (demonstrated in the prototype: single-level `@a[0]=99` went
  through a different path and severed the binding). Likewise, slice/iteration/`.raku`/native-method **element reads scan the items directly**, so
  ContainerRef leaks into them (the entrance of the "deref everywhere" regression).

#### The staged order that was fixed (next session starts here)

Big-bang is impossible. Like Phase 3 Stage 0, **"establish the chokepoints first (behavior-preserving)", then switch to cells**:

- [~] **Stage 0+1 — bind cell-ification of array elements (started; RHS binds landed)**: Stage 0 (write chokepoint) and
  Stage 1 (cell switch) implemented together in 1 PR:
  - **Write chokepoint** `Value::assign_element_slot(slot, val)` (write-through if ContainerRef, otherwise
    replace) newly added, routed into the main array-element write paths (the leaf of `assign_array_multidim`, the single-int / 2-level /
    deep-nested arms of `exec_index_assign_*`).
  - **Read decont** added to `resolve_array_entry` / `resolve_hash_entry` (the single-read chokepoints) (ordinary arrays
    have no ContainerRef elements, so this is a no-op).
  - **Cell switch**: `array_slot_ref` becomes "if the element is a *scalar leaf*, promote in-place to `ContainerRef`; for Array/Hash
    *intermediate* elements keep the traditional `ArraySlotRef` (preserving Arc-shared traversal)". `$x` holds a ContainerRef to the same cell,
    reusing Phase 1's scalar cell mechanism. **Escape-aware**: promotion happens only at `:=` bind = ordinary arrays stay bare values
    (avoids the perf cliff; not repeating the #2746 mistake).
  - **Value-context transparency**: so that a bound-element cell leaking through a native method's raw items read is harmless,
    `to_int`/`to_float_value`/`to_f64`/`arith_*` (reduce etc. fold raw items) were made ContainerRef-decont aware.
  - **Solved**: **RHS element binds** across single/2-level/deep array/hash mixed paths (`$x:=@a[i]`, `$x:=$s[1][1]`,
    `$abbrev:=$struct[1]<key><subkey>[1]`) — writes reach across COW (cell sharing resolves SlotRef staleness).
    nested.t 25→30. `make test` green (native-array-mut.t flakiness is a pre-existing Arc-ptr meta issue, same rate as main ~35%),
    no leaks in common array/hash operations. `t/element-bind-cell.t` 13 cases.
  - **Remaining (next slices)**: LHS binds (`$struct[..]:=$var`, nested.t 7/8/26/28) and element-to-element binds (`$a[i]:=$b[j]`, 31-37) are
    the BOUND_*_REF sentinel / varref mechanisms, handled separately. Cell-ification of hash elements (`%h<k>:=` deep paths) also not done (still HashSlotRef).
  - **Investigation record 2026-06-11 (attempted hash element cell-ification → revert)**: added the array-analogous
    scalar-leaf promotion (`promote_hash_entry_cell`) to `hash_autovivify` / `hash_slot_ref_lazy` and write-through for single hash-key writes (around 3115 in
    `exec_index_assign_expr_named`), but **nested.t collapsed from 30→7 AND hash bind itself failed to write through**, so reverted. Deep binds via arrays
    (`$struct[1]<key><subkey>[1]`) traverse hash intermediate layers via many hash read/write paths, and the hash promotion
    interfered with them. **Lesson**: hash element cell-ification must first (a) consolidate all hash element reads into the `resolve_hash_entry`-family
    chokepoints and (b) route all hash inserts (~15 sites in `vm_var_assign_ops`: 163/1843/2144/2180/2614/2762/2784/3096/3113/3934…)
    through a write-through helper — **otherwise array-through-hash breaks**. The write surface is wider than arrays.
    Order: first establish the hash write chokepoint (behavior-preserving) → then promote; follow the same Stage 0→1 as arrays for hashes.
  - [x] **Hash elements Stage 0 — write chokepoint established (behavior-preserving; landed)**: routed all Raku-hash-value
    element writes through the new helper `Value::hash_insert_through(map, key, val)` (`value/mod.rs`, the hash counterpart of
    `assign_element_slot` = write-through if the existing entry is a `ContainerRef`, otherwise insert/replace). Wired sites:
    `vm_var_assign_ops.rs` 1843 (typed hash element) / 2144 (fast-path single hash key) / 3115 (the plain insert branch of the generic index-assign)
    / 3699 & 3703 (nested hash-in-hash) / 3759 (deep hash-in-hash) / 3934 (deep multidim final level,
    extracting the existing inline write-through into the helper) / 4017 (interior-mutation hash write), plus `value/mod.rs`'s
    `hash_assign_at` / `hash_slot_write`. **Inserts into brand-new empty maps** (2180/3350/3373/3953 etc., plus the post-`contains_key`
    autoviv intermediate layers 3873/3910) have no existing entry and thus can never be a ContainerRef, so they need no routing and are left as-is.
    Currently no hash holds ContainerRef entries, so all calls degenerate to plain insert/replace = **behavior unchanged** (nested.t 30/43
    maintained, `make test` green, `hash_chokepoint_tests` 3 cases).
  - [x] **Hash elements Stage 1 — cell promotion of bound hash elements (landed)**: added the new helper
    `Value::hash_slot_ref(key)` (`value/mod.rs`), symmetric to `array_slot_ref`, and switched the `:=` bind path (the
    `Value::Hash` arm of `exec_index_autovivify_lazy_op`, `vm_var_index_ops.rs`) over from `hash_slot_ref_lazy`. Behavior: **existing scalar leaf** →
    promote in-place to a `ContainerRef` cell (survives across COW); **existing Array/Hash intermediate layer** → keep the traditional `HashSlotRef`
    (do not break deep traversal); **missing key** → stay lazy (create nothing until write). Solved: **deep hash-leaf RHS binds**
    `my $y := %g<outer><inner>; %g<outer><inner> = 55; say $y # 55` (the old HashSlotRef went stale when the inner hash COW-ed → 10).
    - **Over-promotion bug + fix**: when the intermediate `<key>` in `%h<key>()...` is a **Sub value** (callable), the bind context propagated
      all the way to the CallOn invocant and cell-ified `<key>` → `()` could not decont the ContainerRef and aborted with "CALL-ME on Sub"
      (nested.t aborted entirely after test 9). Fix: in `compile_expr_call_on` (`expr_block.rs`), save/false/restore `scalar_bind_autovivify`
      (a call's invocant and args are value reads, not bind targets = same shape as the call-arg reset in escape analysis).
    - **Leak fix**: hash `.values` (`collection.rs`) collected ContainerRefs without deconting → `.sort`/comparison broke
      (`%h.values.sort` yielded `10,2,3`). Decont with `v.deref_container()` (**array `.values` has the same pre-existing leak** = handled separately).
    - Verification: nested.t 30/43 maintained (Failed 7-8/11-12/26/28/31-37 are the separate LHS/element-element mechanisms, unchanged), `make test` green
      (native-array-mut.t 26 is the pre-existing Arc-ptr meta flake, main 3/8 ≈ branch), int.t 0.16s (no perf cliff = escape-aware,
      only `:=`-bound elements are promoted), `t/element-bind-cell.t` 22 cases (hash leaf single/deep + callable non-promotion + iteration non-leak).
    - **Remaining (to Stage 2)**: missing-key lazy binds remain DeferredHashAccess (create & promote on write). Other raw-items reads such as slice/`.kv`/`.pairs`/
      `.raku` may leak cells (rare since limited to bound elements = flush out via CI release roast).
  - [x] **Cell-ification of deep LHS binds (landed; Stage 2 started)**: added bind handling to `$struct[..]<..>[..] := $scalar` (`IndexAssignDeepNested`
    → `exec_index_assign_deep_nested_op`). Previously this handler did not handle bind_mode at all and assigned `val` (the bind payload)
    raw = deep LHS binds were entirely non-functional. The new helper `unwrap_bind_index_value` (payload unwrap) extracts
    `(val, source)`, and only for a **plain scalar source** (no `\0idx\0`): (1) create a `ContainerRef` cell from `val`,
    (2) at the final level store that same cell directly into the element (a fresh `:=` is a replacement, not a write-through), (3) after the loop write the same cell back to the source variable
    slot (`set_env_with_main_alias` + `update_local_if_exists`). Both sides share the same cell = the symmetric form of RHS, reusing the existing
    read (`resolve_array_entry` decont) / write (`assign_element_slot` write-through) / scalar var cell (Phase 1) chokepoints.
    Survives across COW (the old `BOUND_ARRAY_REF_SENTINEL` by-name back-ref went stale with depth).
    - Result: **nested.t 13→7** (fixed 7-8/26/28/31/34), `make test` green, int.t 0.21s, let.t PASS (let/temp × cell OK),
      `t/element-bind-cell.t` 31 cases. Single-level LHS binds (`@a[0]:=$v`, the sentinel path `IndexAssignExprNamed`) left unchanged for now.
    - **Remaining**: (1) LHS binds through `<key>()` (nested.t 11-12, the `IndexAssignGeneric`/method-lvalue paths). (2) element-to-element binds
      (`$a[i]:=$b[j]`, nested.t 32-33/35-37, `\0idx\0`-encoded source).
  - [x] **Cell-ification of single-level LHS binds + array-copy decont (landed)**: migrated `@a[i] := $scalar` (the
    single-int array arm of `IndexAssignExprNamed`) from `BOUND_ARRAY_REF_SENTINEL` to cells. Only for a plain scalar source, store a `ContainerRef`
    cell into the element + write back to the source variable (`pending_source_cell`, same shape as the deep handler). Element sources (`\0idx\0`) keep the sentinel
    for now. Cell binds do not need `mark_bound_index` (the cell itself is the alias). **Side bug fix**: `my @new = @array` (array value copy)
    shared the items Arc in `coerce_to_array` (`runtime/utils.rs`) = **cell elements leaked and were not snapshotted** (writes to `$var` propagated into the copy).
    This was also a **pre-existing leak for RHS binds**, not just LHS (Stage 1 #2902). Only when cell elements exist, decont with
    `deref_container()` (Raku's `=` value semantics; ordinary arrays keep sharing the Arc = perf preserved). Fixed the arrays.t regression + the RHS leak too.
    nested.t 7 maintained, `make test` green, int.t 0.16s, `t/element-bind-cell.t` 35 cases (including LHS/RHS copy-snapshot).
    **Sentinel deletion not yet** (element-source bind (2) and read paths still use it).
  - **Investigation record 2026-06-11 (element-element / container-leaf binds = hardest; prototype reverted)**: attempted to solve nested.t 32-37 and
    `my $x := $foo[1]<key>` (binding to a **container-valued terminal element**). **The implementation plan for a retry (with embedded prototype
    code fragments, remaining work, cycle safety, a leak checklist, and verification steps) = `docs/element-element-bind-plan.md`.**
    Findings from the prototype (subsequently reverted):
    - **Root cause**: a bound terminal element with a container value (`$foo[1]<key>` being a Hash) stays a HashSlotRef and goes COW-stale. Unlike RHS scalar leaves,
      the **container leaf must be cell-ified**. But `array_slot_ref`/`hash_slot_ref` by design do not cell-ify Array/Hash leaves,
      for the sake of deep traversal. → Solved by **terminal-promotion**: identify the outermost (terminal) index of a bind RHS with a compiler flag `bind_terminal`
      + a new op `IndexAutovivifyLazyTerminal`, and promote container leaves to cells only at the terminal (intermediates keep SlotRef). Not applied to `=:=`
      (avoids over-promotion). The terminal flag must be set both at stmt.rs:767 and on the SyntheticBlock/MarkBind path (helpers_block_inline).
    - **However, deep write-through into container cells ripples into every assign handler**: since the cell **holds a Hash/Array**,
      `%h<key><inner>=50` (a deep write through the cell) only lands if the assign handlers decont ContainerRef intermediates/roots and **mutate the
      container inside the cell in-place**. Naively, the match falls into `_=>{}` and **the write is dropped**. As countermeasures,
      adding (a) `assign_into_nested_container` (ContainerRef descent) to the 2-level nested handler and (b) `descend_container_ref` (descending to the Mutex data pointer)
      to the deep handler's raw-pointer walk **solved single-level container binds** (`my $x:=%h<key>` bidirectional OK). But **RHS container-leaf works in only 1 of 3
      directions** (the reverse direction `$x<subkey>[1]=8` fails because root `$x` itself is a cell = the nested/generic handlers cannot read the root as a Hash and the write drops),
      and **element-element remains unsolved**.
    - **Conclusion = why it is the hardest**: container-leaf cells require **ContainerRef-descent in the root reads and intermediate traversal of all
      index-assign handlers (named/nested/deep/generic)** (a "deref everywhere" surface) + **cycle safety** (self-referential binds at nested.t 268+
      can make the descent loop infinite) + **container-cell leak hardening** (iteration/.raku/slice).
      It cannot close safely in a single PR. The terminal-promotion mechanism and the 2 descent helpers are valid, but the full-handler rollout of root-cell descent
      + cycle detection + leak verification needs dedicated work. The prototype had no binding-sweep regressions but did not fix the targets (32-37) and
      `make test` full was unverified, so it was reverted. On retry, proceed in this order: (1) terminal-promotion (2) all-handler root/intermediate descent (with cycle detection)
      (3) container-cell leak hardening (4) release roast.
  - **✅ LANDED (PR #2922 + #2925, 2026-06-12)**: applied the above plan verbatim in Phase 1-5 order and implemented element-element /
    container-leaf / cyclic binds, completing it exactly as `docs/element-element-bind-plan.md` prescribes. **nested.t 43/43 (added to the
    whitelist)**. #2922 covers 32-37 (terminal-promotion + `assign_into_nested_container`/`descend_container_ref` +
    `env_root_descended_mut` root descent in the nested/named handlers + `compile_bind_index_value` terminal
    promotion on RHS + read-side decont + `MAX_DESCENT` cycle guard + `.raku`/`.gist` leak hardening). **True root cause found outside the plan**:
    the intermediate array level's `needs_viv` in the deep-nested traversal destroyed existing `ContainerRef` elements by overwriting them via vivify (the real identity of the
    cyclic off-by-one) → solved by excluding `ContainerRef` from `needs_viv`. #2925 covers 11-12 (`$struct[1]<key>()<subkey>[1]`
    = `is raw` sub mid-path binds): routed the generic handler's Array arm for stack-target IndexAssign through `assign_element_slot`
    so bound cells are written through (direct replacement was destroying the cell). `t/element-bind-cell.t` 35→54.
- [x] **True shared cell for `is rw` params (scalars.t 24/27) = LANDED (#2928)**: `:=` rebinding to an `is rw` param
  is shared with the caller's variable via a live `ContainerRef` cell. take-rw also landed in #2930 (gather.t 38).
  Implementation plan (historical) = `docs/is-rw-shared-cell-plan.md`. scalars.t 33/33.
- [~] **Structural elimination of the Q2 type-meta Arc-ptr flake (2026-06-12, two-pronged)**:
  - **(1) Hash = HashData embedding (full absorption) DONE (#2952)**: embedded
    value_type/key_type/declared_type into `Value::Hash(Arc<HashData>)`, deleting the `hash_type_metadata` side table and
    `reconcile_hash_type_metadata_from_name`. Metadata moves together with COW = mis-inheritance from ptr reuse is gone for Hash.
    Plan and remaining work (embedding original_keys = Stage 2, then rolling the same wrapper out to Array/Set/Bag/Mix) =
    `docs/hashdata-migration-plan.md`.
  - **(2) Weak-guarding the not-yet-wrapped tables (#2953)**: moved to `PtrKeyedMap`
    (`HashMap<usize,(Weak<T>,V)>`) in `runtime/ptr_keyed.rs`: `array/mix/set/bag_type_metadata`, `container_defaults`
    (split into array/hash), the utils globals `shaped_array_ids` and `grep_view_bindings`.
    - **Mechanism**: each entry's `Weak` pins the ArcInner → **address reuse is physically impossible while the entry lives** =
      the intermittent flake where a new container mis-inherits a dead typed container's metadata (the alloc-order-dependent deaths of
      S02-names-vars/perl.t, native-array-mut.t, etc.) is structurally eliminated. Lookups verify `strong_count>0`; inserts sweep dead entries when a threshold
      is exceeded (also resolving unbounded growth and pinned memory). `remove` returns the value even if dead (pre-COW reclamation = for migrate).
    - **Collision-defense hacks removed**: `clear_container_default` (+4 sites), and the 2 defensive post-make_mut
      unregisters in `vm_call_method_mut_ops`.
    - **★ Important side effect and countermeasure**: with a `Weak` held, `Arc::make_mut` **always relocates** even at strong==1 (the weak>0 rule) →
      the implicit assumption "uniquely-owned in-place mutation is pointer-stable" breaks (only for guarded containers). Healed the 3 manifest paths:
      VM native push (`capture_container_meta`/`restore_container_meta` in `exec_array_push_op` = type meta +
      `is default`), the interpreter push arm (`reattach_array_type_metadata`), and **`.WHICH` stability** (2-level nested
      hash writes write the outer in-place via raw ptr when strong==1 — the cast must be `*mut HashData`;
      see the UB lesson in hashdata-migration-plan). **New healing gaps will manifest as "typed/shaped container loses metadata after mutation" or
      ".WHICH changes"** — compare tmp/typed-meta-smoke{,2}.raku / tmp/which-map.raku against main to detect.
    - **The 2 hash original-keys tables (`hash_object_keys`, `hash_original_keys_registry`) are deliberately out of scope**:
      object-hash construction depends on the in-place pointer stability of repeated make_mut (putting the guard on regresses
      .antipairs/.invert of S09-typed-arrays/hashes.t); the right fix is (1) the HashData embedding of Stage 2.
    - **Remaining (unguarded)**: the Seq-family ptr keys (`predictive_seq_iters`, `squish_iterator_meta`,
      the `__mutsu_predictive_seq_iter::` env key) and the `{:p}` embedding in WHICH strings are a separate subsystem where the same hazard remains (small).
    - Verification: cargo test 466 (including 5 PtrKeyedMap unit tests, deterministic tests of stale-inheritance impossibility), make test 727/6588,
      whitelist S09×17 + perl.t×3 + map.t + S02-types/hash.t + let.t + 3467 typed-hash canary tests, int.t 0.163s.
- [~] **Stage 2 — deep `>>++`/`deepmap`/object-hash; removal of the legacy SlotRef mechanisms** (started 2026-06-13, slices in progress):
  - [x] **slice 1 = deepmap container passing (#2964)**: pass leaves to the callable via transient `ContainerRef` cells, and
    after the call write the cell value back in-place to the source Array/Hash slot (no cell remains). Results are deconted to prevent
    cell leaks from identity blocks. Fixed hyper.t 330-333 (12→9 fail; the rest are separate nodal/callable features).
    t/deepmap-container.t (16).
  - [x] **slice 2 = removal of intermediate SlotRefs on the bind path (#2966)**: `exec_index_autovivify_lazy_op` returns
    intermediate Array/Hash elements **as the element value itself (inner Arc shared)** instead of wrapping them in SlotRef back-refs — after #2902-#2925,
    leaf promotion happens in-place inside the shared Arc, so the back-ref was unnecessary. The 2 intermediate arms of `array_slot_ref`/`hash_slot_ref`.
  - [x] **slice 3 = total abolition of BOUND_HASH_REF_SENTINEL**: replaced the 3 creation paths (single hash-key bind named handler,
    slice bind, `.BIND-KEY` ×2) with "store a `ContainerRef` cell into the hash entry + write the same cell back to the source variable",
    and deleted the consumers (the BOUND arms of resolve_hash_entry/hash_has_sentinels/resolve_hash_for_iteration,
    the sentinel write-through arm) and the sentinel constants. Design points:
    (a) **For an element source (`%h<k> := @a[0]`), val is already a LazyTerminal-promoted cell** — store it into the entry as-is
    (the old sentinel was a broken path writing a `\0idx\0` name into env, so hash-key writes never reached the element; fixed
    properly). (b) **Cell-reuse pre-read**: if the source is already cell-bound (`@arr[0] := $x; %h<k> := $x`), share the
    existing cell (overwriting with a new cell would sever the other alias — also a pre-existing bug fix). The `BindSourceCell` type
    pre-reads before the borrow. (c) **Assignment snapshot**: `%new = %hash` deconts cells in `resolve_hash_for_iteration`
    (assignment creates new containers, S03-binding/hashes.t 30). (d) `.push` onto a cell-bound variable with an `@`/`%` source
    goes through the `exec_array_push_op` fallback: decont → dispatch → write back to the cell
    (also fixing the pre-existing failure inherited from #2914 on the array side). t/hash-bind-cell.t (19).
  - [x] **slice 4 = total abolition of BOUND_ARRAY_REF_SENTINEL**: moved the creation of single-level array element binds (the named handler's
    `\0idx\0` element-source branch) to the same shared `ContainerRef` cell as the hash side. Generalized slice 3's `bind_cell` pre-read to be
    target-independent (`hash_bind_cell`→`bind_cell`, consumed by both array/hash arms); array element sources
    (`@x[i] := @b[j]`) also store the LazyTerminal-promoted cell as-is, like hash. All consumers abolished:
    the BOUND arm of `resolve_array_entry` + the `resolve_bound_source` helper, `resolve_bound_array_elements` (sentinel
    scan → cell decont), the `is_bound_index` staleness check (sentinel-presence test → stale cleanup if the element is a non-cell),
    `exec_container_eq_indexed_op` (name-match test of `get_binding_source_of_indexed` → `Arc::ptr_eq` of both
    slots' cells via `raw_element_at_encoded`). Sentinel constants deleted. **Bug fix**: array-mediated write-through
    to a hash-element source (`@y[0] := %src<a>; @y[0] = v` did not reach `%src<a>`, dying with "Cannot modify an immutable value") is resolved
    (matches raku). `mark_bound_index` is skipped for cell-storing binds (the `stored_bind_cell` flag). t/array-bind-cell.t (14).
    **Kept**: the separate `WrapVarRef`/`__mutsu_varref_*` capture mechanism (`\($a)`, `key=>$var` write-through, binding.rs:499/506) can
    be stored into array elements, so the `varref_target` branch is retained.
  - [x] **slice 5 = deep write-through for whole-container sources**: the root cause of `@h[i] := @inner; @h[i][j] = v` not reaching
    `@inner[j]` = on the array-as-outer nested-assign path (vm_var_assign ~3780), when `arr[inner_i]` is a `ContainerRef`
    cell, `needs_viv` (`!matches!(Array|Hash)`) was true → the cell was **overwritten** with an empty array. Fix = exclude ContainerRef from needs_viv
    too + add a ContainerRef arm to the match → delegate to `assign_into_nested_container` (already handles cell descent, array resize, hash
    insert). **A whole-container source has `@inner` rebound to the same cell at bind time** (pending_source_cell),
    so if the deep write follows the cell it also reaches the source-side alias (`@h[i][j]=v`→`@inner[j]`; combined with push and shared multi-sibling
    binds, matches raku). Pinned array-valued/hash-valued sources, hash-key bound, and sibling sharing in t/array-bind-cell.t (20).
  - **Map of remaining slices** — detailed design in **`docs/slotref-removal-plan.md`** (a dedicated doc that pins down the
    role separation of the cells-everywhere problem, phantom entries, the hazard audit, and the staged slices):
    - [x] **slice 1 = cell-ification of computed-target `:=` binds (#2974)**: the `IndexAssignGeneric`
      `HashSlotRef`/`ArraySlotRef`-into-env becomes cells (this path was live and also had a write-through bug, now fixed).
    - [x] **slice 2 = full deletion of the dead `ArraySlotRef` variant**: slice 1 removed the last creation site (4291),
      so variant creation is zero = fully dead → deleted the variant definition, `array_slot_read/write`, and all consumer
      arms. Behavior-preserving. **Important correction**: the original "de-Value-ify the plain write-autoviv cursor" idea
      did not correspond to reality and was dropped — `%h<a><b> = 5` goes via `IndexAssignDeepNested` and was SlotRef-free from the start, and the non-lazy
      `IndexAutovivify` opcode is also dead (see the correction at the top of plan §3).
    - [x] **slice 3 = phantom-entry: cell-ify materialization of missing-key binds**: `:=` binds of missing keys
      (`HashSlotRef`/`DeferredHashAccess`) materialize **on first write** into shared `ContainerRef` cells.
      bound var ↔ hash entry become a bidirectional alias (fixed the **case-C bug** where cross-writes did not propagate after materialization; matches raku).
      Added the `materialize_bound_slot_to_cell` helper; deleted the dead `deferred_hash_write`. `t/phantom-entry-bind.t` (12).
      **Keeping the token is correct**: removing the deferred token from Value would require an in-map phantom representation (intermediates are also lazy, so
      a tombstone is impossible) or a side table (ptr-keyed global = anti-goal) → cell-ified materialization is the substance of phantom.
    - [x] **slice 4 = cell/shared-Arc-ify `hash_autovivify` in bind descent (#2966 family)**: bind-descent
      autoviv becomes `hash_autovivify_cell` (existing cell → as-is / container leaf → shared Arc / scalar leaf → cell promotion /
      missing → empty Hash). Eradicated the `HashSlotRef` back-ref.
    - [x] **slice 5 = final SlotRef kill = merge `HashSlotRef`+`DeferredHashAccess` into a single `HashEntryRef{hash,path}`
      (#3472, 2026-06-23)**: confirmed that deleting all variants is **fundamentally impossible without an anti-goal side table** (the
      deferred-vivification token for missing-key binds demands a Value variant). Via "purging the `SlotRef` concept + merging into a single honest token",
      achieved `git grep 'SlotRef\|DeferredHashAccess' src/`=0. Abolishing the `parent_slot` chain made all missing-path binds of 3+ levels work.
      Details = `docs/slotref-removal-plan.md` §0/slice 5. Pin = `t/hash-entry-ref-deep-bind.t`.
      - **Deliberately not addressed (zero-value; deferred)**: cell-ification of the eager `hash_autovivify` in `is raw` reduce lvalue-reads
        (returns a path-len-1 `HashEntryRef`). **The variant is irreducible due to its deferred-token role, so cell-ifying would not remove it**, and
        the target is a **lenient path that current rakudo itself rejects with `assign requires a concrete object`** (only mutsu accepts it) = no behavior change either.
        Pure internal purity with zero value, so deferred.
    - [x] **grep-rw-view removal (#3466, 2026-06-23)**: under the registration gate, promote matched elements to cells and abolish the view registry
      (a ptr-keyed global) entirely (`for_grep_view`/`GrepView`/`grep_source`/index-based writeback; -90 lines).
      `.grep`'s rw topic writes back to the source via the ordinary element-cell write path. Pin = `t/grep-cell-read-deref.t`.

> **✅ Phase 2 (first-class containerization of array/hash elements) = COMPLETE (2026-06-23)**. Stage 0/1/2 + slotref-removal slices 1-5 +
> grep-rw-view removal all landed. `=`-share / `:=`-bind / element-element bind / deep missing-path bind / scalar-param sharing /
> typed-array sharing / for-rw / deep HoH sharing all match raku (backed by probes + `t/container-identity-phase2-complete.t` 18 / nested.t 43
> / the various share suites). The only remaining non-addressed item is the zero-value reduce-eager-cell purity above (no need to start since the variant
> is irreducible). **Next, Phase 3 (instance attribute cells, below) is the remaining theme of first-class-ification.**

**Mandatory verification (each Stage)**: nested.t, `make test`, **release-roast main-vs-branch comparison** (the lesson of PR #2898, where a
typed-array fix passed make test yet regressed `S06-currying/misc.t` in the release roast = element changes have leaks that only
show in release), and int.t wall-clock (the perf-cliff canary).

### Phase 3 — first-class instance cells (attribute containers + attribute binding)

> User direction (2026-06-10): for the dynamic var / instance mutation across-closure problem, among the fix options
> (A) writeback full-chain scan / (B) exit-writeback by-id / (C) first-class instance cells, proceed with **(C)**.
> Structural solution = the largest rework. Below is the design of (C).

#### Problem to solve (root cause; details in docs/vm-state-ownership.md "Investigation record 2026-06-10")

Because instances are held **by value**, when a mutating method inside a closure frame (`$*ERR.print`, `$obj.set-x`, etc.) runs, the
writeback `overwrite_instance_bindings_by_identity` (`methods_mut.rs:415`) scans `self.env.values_mut()` =
**only the overlay tier of the scoped env**, and never reaches the binding of the same instance held by the caller frame (parent tier =
immutable `Arc<Env>`). Even with the same id, the mutation vanishes when the frame returns. The representative symptom:
in `sub cap(&code){ my $*ERR=F.new; &code() }`, a `note` inside `code` cannot write to the rebound `$*ERR` (pre-existing; reproduced via `&code()`).
map/for only happen to work because they execute **in the same frame** so overlay == caller.

#### Current representation

`Value::Instance { class_name: Symbol, attributes: Arc<InstanceAttrs>, id: u64 }` (`value/mod.rs:1000`).
`InstanceAttrs` **wraps** `attributes: HashMap<String,Value>` **via Deref/DerefMut** (`:455`).
- Sharing: share `Arc<InstanceAttrs>` by clone (CoW).
- Mutation: `Arc::make_mut` (CoW into a separate copy) → propagate via `overwrite_instance_bindings_by_identity` (**scan all env bindings
  matching the id and replace them**). **This scan being overlay-only = the substance of the bug**.

#### Target representation (C)

Make `attributes` a **shared mutable cell**. Cloning `Value::Instance` still shares the cell → mutations are visible to all holders
(caller env / closure overlay / nested attributes) → **abolish the by-id scan of `overwrite_*_bindings_by_identity` entirely**.
- Because of thread crossing (`clone_for_thread`), `Rc<RefCell>` is impossible → **`Arc<RwLock<HashMap<String,Value>>>`**
  (or make `InstanceAttrs` itself an interior-mutable cell while `Value::Instance` keeps holding `Arc<InstanceAttrs>` as before).

#### Blast radius (measured)

`Value::Instance` references: **875**; `Instance { attributes, .. }` patterns: **127**; `make_mut`(methods_mut): **16**.
**Under Rust's borrow model, `Deref` into a locked HashMap is impossible** (guard lifetime). Therefore the 127
`attributes.get(k)`-style read sites must move to API access (lock+clone). A one-shot change is impossible.

#### Staged introduction (avoid big-bang; CI as the safety net at each stage)

- [x] **Stage 0 — establish the encapsulation boundary (behavior-preserving), completed (#2856)**: removed `Deref`/`DerefMut` from `InstanceAttrs` and
      instead added inherent methods with the same signatures as `HashMap` (`get`/`contains_key`/`insert`/`get_mut`/`entry`/`iter`/
      `keys`/`values`) plus owned accessors `as_map()->&HashMap` / `to_map()->HashMap`. All access to attribute storage now
      goes through a method boundary (eliminating Deref leaks). **The internal representation stays HashMap** = byte-identical.
      Removing Deref made the compiler enumerate ~197 sites: deref-coercions (passing `&Arc<InstanceAttrs>` to `fn(&HashMap)`) mechanically replaced
      with `.as_map()`, `(**attributes).clone()` with `.to_map()`. make test 6112 all green, cargo test 458/0,
      clippy green. This localizes Stage 1's representation switch (internals to a shared mutable cell).
- [x] **Stage 1 — representation switch (shared cell-ification), completed**: `InstanceAttrs.attributes` from `HashMap` → `Arc<RwLock<HashMap>>`
      (`AttrCell`). `Value::Instance` keeps holding `Arc<InstanceAttrs>` as before.
  - **read API**: `as_map()` returns an `RwLockReadGuard` (`Deref`→`&HashMap`). Chained calls (`.get`/`.iter`/`for`) are
        unmodified; the ~87 sites passing to `fn(&HashMap)` only need an added `&`. `get()` (making it owned would break all of `*x`/`.cloned()`/`and_then(fn(&Value))`)
        was **deleted**, and all `.get()` were lowered to `.as_map().get()` to preserve borrow semantics (504 sites mechanically replaced).
  - **mutation**: `insert`/`insert_if_absent`/`with_attr_mut` are `&self` (in-place under the write lock). `make_mut(attributes)`
        sites become direct `&self` mutation.
  - **Cross-frame visibility (the substance of the bug fix)**: added a new `id→Weak<cell>` registry (`instance_cells`).
        `make_instance_with_id` **reuses** an existing live cell (writes the map and returns an alias-sharing instance).
        `overwrite_instance_bindings_by_identity` **writes directly into the live cell** via `update_instance_cell(id, &updated)` before
        scanning → mutations reach aliases in caller frames the scan never visits. This resolves `note`/$*ERR and closure-mediated
        instance mutation. The registry is scaffolding to be removed together with the scan in Stage 2.
  - **Preserving `.clone` (Raku's independent copy)**: hand-wrote `InstanceAttrs::clone` as a **deep copy (new cell, queue_destroy=false,
        unregistered)**. Sharing happens only via `Arc<InstanceAttrs>` (Value clone). With this, the ~30 `(*attributes).clone()`
        writeback sites keep independent-copy semantics unmodified. `temp`/`let` saves needed the same kind of independent snapshot
        (`into_temp_snapshot` deep-copies the instance; restore writes back **into the live cell** via `make_instance_with_id`,
        preserving identity and alias visibility) — neglecting this breaks temp restore in `t/lvalue-method-rw.t` (actually hit and fixed).
  - Verification: cross-frame note/closure mutation, alias sharing, `.clone` independence, `===`/`.WHICH`/`eqv` identity, DESTROY (once),
        temp/let restore — all match raku. clippy green.
- [ ] **Stage 2 — make the cell the single source of truth (keystone) + abolish all propagation hacks**: the user's decision (2026-06-10) initially assumed a **single PR** →
      it turned out that **wiring cell writeback for in-place mutation of array/hash attributes (`@!a.push`/`%!h<k>=`) is a separate-project-sized job**, so
      the user re-decided (2026-06-10) to **slice by sigil** (scalars first).
  - [x] **Stage 2a — wire scalar attributes (`$!x`/`$.x`) directly to the cell (landed: branch `phase3-stage2-scalar-cell`)**:
        reads of `Var("!x")`/`Var(".x")` become direct reads of `self`'s shared cell (`read_self_attr_cell`, placed after the atomic/CAS check = CAS keeps
        the traditional `shared_vars` to avoid livelock). Write paths — `exec_set_local_op`/`exec_assign_expr_local_op` (wrapped with
        `mirror_attr_local_to_cell`), name-based `exec_assign_expr_op` (`$.x = v`/`$!x = v`, `mirror_attr_value_to_cell_by_name`),
        post/pre inc & dec (`sync_attr_local_from_cell_by_name` → execute → mirror) — all write to the cell. **Scalar writeback
        removed** (`writeback_attributes*` specialized to array/hash only; `AttrSlots::private/public` deleted). Instead, at method exit, while env/locals are live,
        `reconcile_scalar_attrs` finalizes the attr map: "env/local changed from the entry snapshot → take that value; unchanged → take the live cell
        value (adopting cross-frame/cell-direct mutations)" and feeds it to make_instance_with_id. This delivers to the cell the 3 paths that write env without
        going through the cell: **attributive params (`method m($!s)`), no-twigil sigilless attrs (`has $x`), and `is rw` accessor
        writes**. **Cross-frame scalar mutation bug fixed** (`self.bump` is visible to the caller's `$!x`; 11 vs old 10). Array/hash attributes,
        CAS, registry/scan/detached/`instance_cells` kept until Stage 2b (cannot be deleted yet).
        Verification: make test all green; S12-attributes/instance & native, S12-methods/{attribute-params,lastcall,defer-call,defer-next},
        S17-lowlevel/cas (whitelist) PASS. Inherited same-name privates (Parent/Child `$!p`) behave the same as main — a pre-existing bug, not a regression.
  - [x] **Stage 2b — wire array/hash attributes (`@!a`/`%!h`) directly to the cell (landed: branch `phase3-stage2b-array-hash-cell`)**:
        reads go via `GetArrayVar`/`GetHashVar` as direct reads of `self`'s cell (generalized `read_self_attr_cell` to all 6 twigils =
        `scalar_attr_twigil_base`→`attr_twigil_base`, returning `(bare, is_private)`). Wired an env→cell mirror
        (`mirror_array_hash_attr_to_cell`) **after** each mutating op: `CallMethodMut`/`CallMethodDynamicMut`/`ArrayPush`/`IndexAssignExprNamed`/
        `MultiDimIndexAssign`/name-based `AssignExpr`. **Crucially, take an env snapshot before the op** (`array_hash_attr_env_snapshot`) and
        mirror only when env actually changed: non-mutating methods (`@!a.join`) also come through `CallMethodMut`, so unconditionally mirroring
        a stale env copy would clobber cross-frame cell mutations (demonstrated and fixed in cf3). **Unified the scalar reconcile across all sigils**
        (`reconcile_scalar_attrs`→`reconcile_attrs`, rule: "cell changed since entry → cell wins; otherwise adopt env changes" =
        cross-frame/per-op mirrors let the cell win; attributive array params `method m(@!a)` and sigilless let env win).
        With this, **array/hash writeback (`writeback_attributes*`) is fully removed** (the `AttrSlots` struct + `compute_attr_slots` also deleted, net -115 lines).
        **Cross-frame array/hash mutation bug fixed** (a nested method's `@!a.push`/`%!h<k>=` visible to the caller).
        Verification: make test all green; S12-attributes/methods, S14-traits, S17/cas (whitelist) PASS; Stage 2b edges 21/21
        (push/pop/shift/unshift/element assign/non-`:delete`/whole-assign/cross-frame array+hash/attributive param/`.=`/chain/read-no-clobber).
        Known unsupported (pre-existing, not regressions): `%!h<x>:delete` (DELETE-KEY goes via an Index target outside the mirror), the space-less
        parse of `@b[0]=v`, inherited same-name privates. **materialize's array/hash insertion / the by-id scan (`overwrite_instance_bindings_by_identity`) /
        the registry (`instance_cells`) / detached are not yet removed** = Stage 2c (next) abolishes them all on the grounds that the cell is the single source.
  - [~] **Stage 2c — abolish all propagation hacks (started; slice 1 done)**: the cell is now the single source for all attrs, so remove the propagation scans.
        - [x] **slice 1 — remove the by-id env/locals scan (branch `phase3-stage2c-drop-byid-scan`)**: **deleted entirely** the
              `for bound in self.env.values_mut()` walk of `overwrite_instance_bindings_by_identity` + the recursive helper `overwrite_instance_recursive`
              (the Instance/nested-attr/Mixin/ContainerRef arms), and the locals walk `overwrite_instance_in_locals` (+ 6 call sites). Propagation is consolidated
              into the single `update_instance_cell(id, &updated)` (direct write to the live shared cell). **Rationale**: after Stage 1/2a/2b, all aliases of an
              instance (same frame, caller frames, `ContainerRef` boxed captures, role `Mixin`s, nested attributes of other instances) share the
              `Arc<InstanceAttrs>` reference and **see the same cell** (deep copy only on explicit `.clone`) → the holders the scan used to rebuild already shared
              the same cell, so the scan was observationally a no-op. `_class_name` is vestigial (the cell is keyed by id) but the signature is kept to
              avoid ~40 call-site churn (removed in a later slice). Verification: make test 6247; S12/S14/S17 whitelist
              106 files, 2111 tests PASS (cross-thread cas, all role/mixin, cross-frame note/closure, nested, escaping-closure
              scalar-holding-instance all green); clippy green. Net -108 lines.
        - **materialize/reconcile removal (user's choice 2026-06-11, staged PRs)**: reconcile rewrites `attributes` (the entry snapshot) to the final values,
              merging the 2 paths that bypass the cell: (a) attributive params (`method m($!x)`), (b) sigilless `has $x` (compiled to bare `Var("x")`,
              distinguishable as an attribute only via the runtime alias table). `is rw` accessors etc. already reach the cell. Simply removing reconcile would let the
              caller's cell writeback roll the cell back to the snapshot, so ultimately replace `reconcile → attributes = base.cell.to_map()` (cell as single source).
              An exit mirror would cause the stale-clobber demonstrated in 2b, so write-time cell routing is mandatory. → Split into 3 stages: (i) attributive params,
              (ii) sigilless cell-direct (equivalent to 2a), (iii) reconcile/materialize removal.
          - [x] **(i) attributive params → cell (landed: branch `phase3-stage2c-registry-removal`)**: right after binding,
                `mirror_attributive_params_to_cell` (reusing `mirror_attr_value_to_cell_by_name`; scalar/array/hash, all twigils) mirrors the params into
                self's cell. **Also excluded attributive params from the read-only fast path** (added `attr_twigil_base(pd.name).is_some()` to
                `has_complex_params`) —— attributive params mutate attributes but the fast path drops writeback, fixing the bug where the mutation vanished after
                `$obj.m($!x)` (`method set-and-read($!x){ say $!x }` read the stale cell 99 instead of the in-body 7, and the attribute stayed 99).
                Verification: make test 6247; S12/S14/S17/S06 whitelist 140 files, 2890 tests PASS; named `:$!x`/BUILD/array/hash params,
                in-body reads, cross-frame all match raku. reconcile kept until (ii)(iii) (the attributive-param path is now redundant with the mirror but harmless).
          - [x] **(ii) sigilless `has $x` made cell-direct (landed: branch `phase3-stage2c-sigilless-cell`)**: bare `Var("x")`
                (sigilless attrs) is routed to the cell via the runtime `__mutsu_sigilless_alias::` table. **read**: extended `read_self_attr_cell` to go through
                `canonical_attr_twigil` (direct twigil → as-is / bare sigilless → follow the alias chain and resolve the `!x` twigil).
                Discovered that sigilless `$x` is read via **GetGlobal** (not GetLocal) since it is not a method-local declaration → also added a cell-direct read to the
                GetGlobal handler in vm.rs (before the env read). **write**: added `write_self_attr_cell` to each alias target of the 6 alias-chain
                propagation loops (inc/dec 4 + name-based assign 1 + SetGlobal 1), mirroring the attr-twigil alias (`!x`) to the cell. The 4 inc/dec
                loops were duplicated, so factored them into the common helper `propagate_sigilless_alias_chain` (cell mirror included). **perf**: the read path is
                hot, so gated by a new flag `Interpreter::sigilless_attrs_active` (a process-sticky bool set when materialize installs a sigilless alias)
                → non-sigilless programs (the vast majority) pay only a string check. **Bug fixed**: same-frame/nested-frame sigilless reads were reading the
                entry env copy (stale) (`method outer { self.inner; $y }` returned 10 where it should return 99 after inner's mutation).
                Verification: make test 6247; S12/S14/S17/S06/S32-num whitelist 171 files 6432, S02/S03/S04 318 files 32004 PASS;
                int.t perf 0.085s (no regression); nested-read/same-method write-read/inc-dec/closure/multi-attr all match raku.
                reconcile kept until (iii) (the sigilless path is now cell-direct so reconcile case-2 is redundant but harmless).
          - [x] **(iii-a) simplify reconcile to cell-single-source (landed: branch `phase3-stage2c-reconcile-removal`)**: removed
                `reconcile_attrs`'s entry-snapshot-vs-env value comparison (the case-1 cell-changed / case-2 env-changed priority decision + sigil-by-type twigil
                inference) in favor of `*attributes = base.cell.to_map()` (the cell is the single source). All plain writes (2a/2b/2c-i/ii) land in the cell at
                write time, so the cell snapshot is the final value. **Exception = `:=`-bound attributes** (`$!x := $outer`): they hold an external ContainerRef in
                env/locals and bypass the cell — **a third bypass** (discovered via has-attr-binding.t). After cell.to_map(), walk each attr key in env/locals
                (bare sigilless + 6 twigils) and if it is a ContainerRef, adopt it preferentially, preserving the `:=` alias across method exit. Verification: make test 6260;
                S12/S14/S17/S03-binding/S06 144 files 2994 PASS; has-attr-binding 6/6; all bypass/sigilless/attrparam tests match raku. (The 2 failures in
                S32-temporal/DateTime.t are the pre-existing timezone bug of `Date.today`=UTC vs `DateTime.now.Date`=local; identical on a baseline with the changes
                stashed = unrelated, an artifact of local JST only; on CI=UTC both agree and it passes.)
          - [x] **(iii-b) remove the slow-path materialize's attr-value env copies (landed: branch `phase3-stage2c-materialize-removal`)**:
                removed the attr-value env copy insertion (~30 lines: `!attr`/`.attr`/`@!attr`/`@.attr`/
                `%!attr`/`%.attr`) on the complex-param method path (`call_compiled_method`). Reads are all cell-direct (2a/2b/2c-ii), exit reconcile is cell.to_map()
                (iii-a), so it was redundant. The sigilless alias table setup + `is default` registration are kept. Verification: make test 6265; S12/S14/S04/S03-binding/S06
                144 files 3076 PASS; cases where closures capture and read/write private/array/hash/sigilless attrs (returning readers/writers, nested+closure, map/gather)
                all match raku.
                ~~**The fast-path (`call_compiled_method_fast`) array/hash env copies are kept**~~ → **removed in (iii-c) (below)**.
          - [x] **(iii-c) pre-op cell sync for mutating ops + fast-path env copy removal (branch `phase3-attr-cell-presync`)**:
                extended `array_hash_attr_env_snapshot` (the pre-snapshot at every mutating-op site) to "**refresh env/locals from self's live cell**
                first, then return the cell value as the pre" (no-op if the Arc ptrs match; `:=` ContainerRef keeps the legacy behavior).
                This eliminates the accident of mutating and mirroring a closure-captured env copy (a stale snapshot from closure creation time),
                **fixing the pre-existing bug "via a closure, `%!h{$k}=$v` keeps only the last write (the first write reads `(Any)`)"**.
                Each closure call now starts from the live cell value. After the fix, removed the array/hash attr env copies in
                `call_compiled_method_fast` (the ones kept in iii-b) — reads are cell-direct (2b) and mutation pre-op sync fills env, so they are unneeded.
                **Bonus: wired the same pre-sync + mirror into `DeleteIndexNamed` (`%!h{$k}:delete`)**, also fixing Stage 2b's
                pre-existing gap "DELETE-KEY is outside the mirror and never reaches the cell". Regression test `t/closure-attr-element-write.t` (16).
        - [x] **registry removal (done, branch `phase3-drop-instance-cell-registry`)**: **abolished entirely** `instance_cells` (the `id -> Weak<cell>` global
              Mutex registry) + `register_instance_cell`/`lookup_instance_cell`/`update_instance_cell`/`overwrite_instance_bindings_by_identity`.
              Converted all ~98 writeback/rebuild sites (`overwrite_*` 47 + rebuild `make_instance_with_id` 51) to 3 new helpers that mutate the receiver's
              `Arc<InstanceAttrs>` cell directly in place:
              `InstanceAttrs::commit_attrs(map)` (in-place write to the live cell, via the deadlock-safe `write_cell_respecting_reads`) /
              `Value::instance_sharing_cell(attrs, class, id)` (rebuild sharing the Arc; a new InstanceAttrs sharing the cell only when the class changes) /
              `Value::write_back_sharing(attrs, class, map, id)` (commit + share combined). **Deleted the id→cell reuse branch from `make_instance_with_id`**
              (from now on fresh-cell only = only for genuinely-new / sentinel ids). Sites that held only snapshot+id (`proxy_store`, buf writes, MRO multi
              invocant, etc.) were solved by threading the receiver's Arc through the call chain. `make_instance_detached` became equivalent to `make_instance_with_id`
              after registry removal but the name is kept as an intent marker for CAS atomic-sync (→ delegates to `make_instance_with_id`). Removed the `_class_name` vestigial param.
              Side effects: resolved the global Mutex registry's Weak entry accumulation (memory leak) and the latent bug of id-0 sentinel Supplies mis-sharing cells.
              Verification: cargo test 461, clippy green, whitelisted S12/S14/S17/S32-temporal/buf 40 files + cross-frame/closure/temp-let/proxy/buf/iterator/grammar all green.
        - [x] **CAS cell-CAS-ification (branch `phase3-cas-cell`)**: migrated instance-attribute cas/atomic ops entirely from the shared_vars side channel
              (`!attr::{id}` value keys / `__mutsu_atomic_attr::{id}::{attr}` / `__mutsu_instance::{id}` parent reclamation + env scan-replace /
              `make_instance_detached` / `sync_atomic_attribute_to_instance`) to **atomic primitives directly on the cell**.
              Added `InstanceAttrs::compare_and_swap(key, matches, new)` (compare+store under a single write lock) / `fetch_update(key, f)`
              (RMW under a single write lock, for atomic add/inc/dec), and gave `builtin_cas_var` (3-arg/2-arg),
              `builtin_atomic_update_unit`, and `builtin_atomic_{add,fetch_add,fetch,store}_var` an attr-cell branch
              (`self_attr_cell_target`: `!x`/`.x` → self's cell + qualified key resolution). Stage 1's livelock is resolved now that
              cell-direct reads (2a) are complete (confirmed by 5 consecutive cas.t PASSes).
              **Discovered a second lost-update race**: the caller wrote back the method-exit reconcile snapshot whole-map via
              `cell.commit_attrs(new_attrs)` every time → TOCTOU-clobbering concurrent cell-RMWs
              (8 threads × `$!count⚛++` ×500 gave 2448/4000. On main it did not propagate to the parent at all = **0**).
              **Fix**: `reconcile_attrs` returns a bool "were there adjustments beyond the cell snapshot (`:=` ContainerRef reclamation)?",
              propagated to callers as `(Value, HashMap, bool)` — if unadjusted, **skip** the exit commit / `write_back_sharing`
              (snapshot==cell so it is a no-op, and it is the race source). Commit only when adjusted (`:=`).
              Side effect: the redundant whole-map write at every method exit is gone (perf). Regression test `t/cas-cell-attr.t` (14,
              including 4-thread increment / 2-arg cas / parent visibility after await; matches raku).
  - The old "single-shot" design (below) was split into Stage 2a/2b/2c. The following is a reference map.

  #### Current mechanism (full map established by CI investigation)
  A method frame holds instance attrs as **env/locals copies**, and the cell is synchronized only via writeback:
  - **materialize (method entry)**: `vm_method_dispatch.rs:~380-430` inserts, per attr, env keys `!attr`/`.attr`/`@!attr`/`@.attr`/
    `%!attr`/`%.attr` as **value copies**. Privates prefer `qualified_key = "{owner_class}\0{attr}"`
    (Parent/Child same-name disambiguation under inheritance). `is default(...)` is registered via `set_var_default("!attr")`.
    Sigilless aliases (trait handles) are bidirectional via `__mutsu_sigilless_alias::`.
  - **read (body)**: `$!attr` compiles to a **plain `Var("!x")`** → `exec_get_local_op` (`vm_var_assign_ops.rs:4131`) returns
    `locals[idx]` (dual-store-synced from env `!attr`). cas dirty prefers env (comment at 4147).
  - **write (body)**: `exec_set_local_op` (4281) / `exec_assign_expr_local_op` (5480). `$!attr=` on a type object dies.
    Intertwined with many contexts: bind/rebind/constant/vardecl/decont-marker, etc.
  - **writeback (method exit)**: `writeback_attributes_from_locals`/`writeback_attributes`
    (`vm_method_dispatch.rs:586/607/1088/1105`) aggregate `!attr` from locals/env into the instance attrs → rebuild.
  - **cas side channel**: for `$!attr`, cas's source of truth is env `!attr` + `shared_vars` (`__mutsu_atomic`/`__mutsu_instance::`);
    the cell is synchronized separately via `make_instance_detached` (detached in Stage 1 to avoid the race).

  #### Keystone (implementation policy)
  Wire the read/write of `$!attr`/`$.attr`/`@!`/`@.`/`%!`/`%.` **directly to self's cell**:
  - Add an attr-twigil check to the var handlers (`attr_twigil_base(name) -> Option<&str>`: `!x`/`.x`/`@!x`/… → `x`), and
    read from / write to the cell of env `self` (Instance). For inherited qualified keys, either hold `owner\0attr` on the cell side, or
    carry owner_class in the frame and resolve (reproduce materialize's priority order in the cell read).
  - **Remove materialize** (stop inserting env copies), **remove writeback** (`writeback_attributes*`),
    **cell-CAS-ify cas** (make the cell's write lock the atomic primitive. Stage 1's livelock happened because reads went through env copies
    = it resolves once reads are cell-direct. This is the verification case for cell-direct reads).
  - Once these are in, abolish entirely: **`overwrite_instance_bindings_by_identity` + the by-id scans (17 files, ~50 sites) / the `instance_cells`
    registry / `HELD_READ_CELLS`+deferred-write / `make_instance_detached` / `update_instance_cell` / CoW `make_mut` ×16**.
    `temp`/`let` cell write-back also becomes straightforward.

  #### Risks / verification
  - This touches the core of the hot path (var read/write) and the dual store (locals↔env sync) = Stage-1-class blast radius. Assumes CI iteration.
  - Mandatory verification: inherited same-name attrs (Parent/Child `$!priv`), container type metadata for `@!`/`%!`, sigilless aliases (trait handles),
    `is default`/`.VAR.default`, rw accessors, cas (4000 under cell-CAS), cross-frame, temp/let, DESTROY.
  - perf: a cell read is heavier than reading an env copy (fetch self + read-lock + clone). Stage 3's escape analysis rescues by
    reverting non-escaping instances to bare values.
- [ ] **Stage 3 — finishing / perf**: use escape analysis to "omit the cell for instances that are neither captured nor `.clone`d" (hot-path rescue;
      same shape as Phase 1 scalars). Also move the type-meta side tables' Arc-ptr keying onto the cell and abolish it (absorbing the Q2 flake).
      **Re-evaluation (settled by the 2026-06-12 perf measurements)**: **neither sub-item is worth starting at this point**.
      - **Escape-analysis cell omission is not supported by the numbers**: in the release profile of bench-class heavy, the cell's
        RwLock **does not appear** in the self-time top 30. The dominant cost is the **allocator ~48%** (`_int_malloc` 18% +
        `memmove` 12% + realloc/free) = `Value::clone` + HashMap churn. The startup-corrected effective ratio is
        bench-class **~4x** / method-call **~5.5x** raku (2.1x/2.2x including startup; mutsu startup 0.01s vs
        raku 0.09s). Allocation reduction comes before cell omission.
      - **The instance side of type meta already has stable keys**: `instance_type_metadata` is a `HashMap<u64(instance id), _>`,
        not Arc-ptr (unrelated to the flake). The root of the Q2 flake is the Arc-ptr keyed side tables of Array/Hash/Set/Bag/Mix
        → **the territory of element cells, Phase 2 (Track B)** and cannot be handled in this Phase 3.
      - **Cheap perf candidates found in the profile** (the de-facto replacement of Stage 3):
        (a) `reset_atomic_var_key_decl` (self 3.7%) — a `format!` + shared_vars **write lock** on every VarDecl,
        even when atomics are unused. Just gate it with the existing `atomic_var_seen()` process-sticky bool.
        (b) `reconcile_attrs` at method exit does `cell.to_map()` (full clone of the attr map) on every call — the returned
        HashMap is by now used only by proxy_fetch and the (gated) commit. Make it lazy / clone only when needed.
        (c) The rest is allocation reduction for `Value::clone`/env inserts = a larger independent project.

#### Mandatory correctness audit (bundle with the switch)

- **`.clone` (Raku's independent copy) creates a new cell (deep copy)**: with shared cells, clone would share the cell and
      `$b = $a.clone; $b.x = 1` would pollute `$a`. The `clone` method (and the independent-copy semantics of `but`/mixins) must
      **explicitly duplicate the cell**. Reproduce the semantics that the current "value clone = independent copy" gave for free.
- **Dynamic vars are not lexically captured**: the captured-env merge in `call_compiled_closure` (`vm_closure_dispatch.rs`
      ~230) puts a captured (stale) `$*ERR` into the overlay, shadowing the live parent. Exclude twigil `*` from capture.
      (This is needed on the read side even with cell-ification alone.)
- **Control-flow name exclusion**: when re-enabling lexical `&`-var dispatch in the future, `return`/`take`/`emit`/`callsame`/… are
      handled directly by the match in `call_function`, so exclude them from dispatch (preventing `&r=&return` infinite recursion; several names
      are not in `is_builtin_function`).
- **eqv / WHICH / comparison**: instance identity comparison (`===`/`.WHICH`) stays id-based (id is invariant under cell sharing).
- **Serialization / precompilation**: cells, like ContainerRef, are not preserved across processes. Check that the per-instance state path
      equivalent to `closure_captured_state` is not broken (the precompilation-regression lesson of [[project_lever_b_slice63_prep]]).

#### Roast tests / symptoms this solves

- `$!x :=` / per-attribute container templates (S03-binding/attributes, S14-traits/attributes 5-8).
- `note`/dynamic-handle writes inside closure-provided blocks (the test-harness pattern `sub cap(&code){ my $*ERR=...; code() }`).
- Mutating methods on any caller-held instance inside a closure.
- Removes the prerequisite blocking Track A lexical `&`-var dispatch ([[project_lexical_amp_var_blocked]]).

#### First slice

Start with **Stage 0 (compat read API + migrating the 127 read sites, behavior-preserving)**. Once that is in, Stage 1's representation switch
becomes a reviewable unit. Stage 0 itself has zero functional change, so CI is almost certainly green and it can be landed large, safely.

---

## 5. Workarounds deletable through unification (the maintainability win)

As container unification progresses, the following scattered hacks become **unnecessary** (= deletion targets):
- the bidirectional dual-store env↔locals sync (coupled with lever B)
- the Arc-pointer-keyed type-meta side tables (the PLAN Q2 item; root of the flake. Carrying type meta on the container removes them)
- the ad-hoc itemization flags (the duplication of `ArrayKind::ItemList/ItemArray` and `Value::Scalar`)
- grep-rw-view binding (`Arc`-pointer keyed; has the bug of surviving across `=` assignment)
- name-based writeback reconcile (the rw aliases of map/grep)

## 6. Guaranteeing speed

- **Escape analysis** omits containers (locals that are never captured, `.VAR`ed, or aliased stay bare values).
- Arrays are **COW**, so reads are clone-free.
- decont is a single branch and predicts well.
- With mid-term NaN-boxing shrinking the payload to 8 bytes, cells become cheap too.

---

## 7. Inventory of value-read opcodes (Phase 0.5 groundwork / Phase 1 blueprint)

A preparatory inventory for establishing the "values pushed onto the stack are always deconted" invariant in Phase 1. For each value-read
opcode, it contrasts **the container shapes it can currently push** with **the Phase 1 goal**. `GetLocalRaw` is the existing precedent (anchor)
for "an lvalue read that pushes the raw cell"; Phase 1's new lvalue opcodes
(`GetLocalContainer`/`IndexContainer`) generalize it.

| opcode | handler | shapes it can push today | current deref | Phase 1 goal |
|--------|---------|-----------------|-----------|-------------|
| `GetLocalRaw` | `vm_var_assign_ops.rs:exec_get_local_raw_op` (~4101) | **raw cell** (no deref/descalarize; does not resolve DeferredHashAccess/HashSlotRef either) | none (intentional; for `=:=`) | **design template for lvalue opcodes** |
| `GetLocal` | `vm_var_assign_ops.rs:exec_get_local_op` (~4108) | ContainerRef is `into_deref`ed (consolidated in this PR). Scalar/ItemArray pass through | ContainerRef→inner (single level) | always deconted |
| `GetGlobal` | `vm.rs:851-1078` (inline) | same as above | ContainerRef→inner (single level) | always deconted |
| `GetArrayVar` | `vm.rs:1079-1138` (inline) | only Hash→Pairs conversion. Scalar/ContainerRef/ItemArray pass through | none | always deconted (**actual behavior change; next stage**) |
| `GetHashVar` | `vm.rs:1139-1201` (inline) | passes through | none | always deconted (next stage) |
| `Index` | `vm_var_index_ops.rs:exec_index_op_with_positional` (~265-269) | Scalar is unwrapped, ContainerRef/ItemArray pass through | partial (Scalar only) | always deconted (**actual behavior change; next stage**) |

**The consolidation surface for ContainerRef value reads is exactly 2 sites** (`GetLocal` / `GetGlobal`). The other
`arc.lock().unwrap().clone()` occurrences are either a different axis (LazyList cache) or read-modify-**write** (the increment sites
`vm_misc_ops.rs:879/927/956/1004`, `vm_var_assign_ops.rs:1468/1514/1606/1652` — they write back to the same arc, so consuming
the arc with into_deref would break them) and are **not value reads**. Hence out of scope for this PR's consolidation.

**Behavior changes that land in the next stage (bundled with Phase 1)**: changing `GetArrayVar`/`Index`/`GetHashVar` to "always push
deconted" produces observable behavior changes in cases where array elements hold `Value::Scalar`/`ContainerRef`/`ItemArray`
(`$(...)`/`.VAR`/itemization). "When to decont and when to keep the container" presupposes Phase 1's
scalar-container semantics, so it will be done in the same PR as Phase 1, together with the real wiring of the new lvalue opcodes.

## Progress log

(appended per PR)

- 2026-06-06: Ledger created. Recorded the current 3 representations and the full trace of the `:=` flattening bug. No code changed.
- 2026-06-08: **Phase 0 started (decont helper consolidation; behavior-preserving)**. The 3 axes (Scalar / ContainerRef /
  ArrayKind itemization) are distinct types with distinct semantics, so the decision was **not to fuse them** but to establish a canonical helper
  per axis and consolidate the scattered ad-hoc unwrapping (fusing would break the Pair check in `is rw` writeback and the `@a=$l` flattening).
  - PR1 (#2736): renamed `Value::decontainerize` → `descalarize` + added owned `into_descalarized` + a decont
    family doc. Deleted the duplicated recursive `strip_scalar`, consolidated the 4 recursive `while let Scalar` loops in
    `methods.rs`. Documented in comments the difference between `OpCode::Decont` (single level) and the recursive helper.
  - PR2 (#2737): consolidated the recursive ad-hoc unwrapping of single-Scalar-arm functions into `descalarize`/`into_descalarized`
    (ops' bag/mix multiply, utils' coerce_to_hash/numeric/set/quanthash, methods_narg's 1arg/2arg).
    Single-level sites, `.VAR`-guarded dispatch, mixed-axis (eqv/truthy/isa), and the exhaustive-match
    value_type_name were deliberately kept inline.
  - PR3: the ContainerRef read axis. The existing `deref_container` always clones, so it is unsuitable for in-place read sites (which lock and
    use `&inner`) (adds a clone), and fatal for hot read opcodes (clone on every read).
    So a **non-cloning canonical reader `Value::with_deref<R>(&self, f) -> R`** was added, `deref_container` was redefined
    on top of it, and 6 in-place read sites were consolidated (utils value_type_name / introspect dispatch_what /
    display to_string_value / the ContainerRef arms of types eqv, truthy, isa_check, what_type_name).
    The hot read opcodes (the move-or-clone of GetLocal/GetGlobal) and Group B (bind/identity/write-through) are
    postponed to Phase 0.5 / Phase 1 (status quo).
  - Behavior invariance confirmed for all of these: `make test` PASS (5274), and the binding/flatten/set/bag/mix/eqv roast
    plan/ran/failed counts match baseline (pre-existing non-whitelist failures unchanged).
- 2026-06-08: **Phase 0.5 stage 1 (behavior-preserving groundwork)**. Per user decision, adopted the staged approach — the behavior-changing
  stack invariant (auto-decont of `GetArrayVar`/`Index`) and the real wiring of the new lvalue opcodes are deferred to the next stage bundled with
  Phase 1; this PR is only groundwork that can be carved out safely:
  - **Added `Value::into_deref(self)`** (the owned move-through version on the ContainerRef axis; completes the `deref_container`↔`into_deref`
    pair, same shape as `descalarize`↔`into_descalarized`). Consolidates the part PR3 explicitly postponed as "hot read-opcodes
    GetLocal/GetGlobal move-or-clone … status quo".
  - Consolidated the hand-written inline `arc.lock().unwrap().clone()` of `GetLocal` (`vm_var_assign_ops.rs` ~4206, keeping the early-return
    structure with the `is_container_ref()` gate) and `GetGlobal` (`vm.rs` ~1071) into `into_deref`.
    Non-ContainerRef keeps the move (hot-path behavior/perf fully unchanged).
  - Added the value-read opcode inventory table to §7 (Phase 1 blueprint; `GetLocalRaw` as the anchor precedent for lvalue reads).
  - Behavior invariance confirmed: build/clippy/fmt PASS. binding/`.VAR`/`=:=`/sigilless t/ + S03-binding roast all PASS.
    Full roast delegated to CI.
- 2026-06-08: **Phase 1 slice 1 (broad boxing of non-loop sibling closures) attempted → reverted (important lessons)**.
  Tried the approach of removing the loop-only gate of `box_captured_lexicals` and `ContainerRef`-ifying captured+mutated `$` scalars outside loops
  too, but it turned out to be **under-scoped on both performance and correctness** and was reverted. Reasons recorded (design constraints for the next slice):
  - **(1) The mutation-writeback gap (partially addressable)**: `into_deref` (PR #2742) made only **reads** cell-aware, but
    **mutation write-back** was not cell-aware. Concretely, `overwrite_instance_recursive`
    (`runtime/methods_mut.rs`, the chokepoint that propagates a mutating method's result to all env bindings by instance identity) did not
    look inside `Value::ContainerRef`, so when a boxed scalar held an instance and received a mutating method, the mutation vanished
    (`my $x; lives-ok { $x = Obj.new }; lives-ok { $x.mutate }; $x.read` — a **test-helper pattern ubiquitous in roast**;
    submethods.t / S24-testing / test-util etc. regressed). Confirmed fixable by adding a `ContainerRef` arm, but
    that is merely the entrance to "a broad audit making all mutation paths cell-aware".
  - **(2) Severe performance regression (the decisive strike against the approach)**: broad boxing turns **on every closure creation** all
    captured+mutated free variables into `Arc<Mutex>` and inserts them into env (= inducing deep clones of the env COW). The loop-only restriction was
    not just correctness but also **an upper bound on boxing cost**. After removing it, `roast/S32-num/int.t` slowed **from 1 second to 150s+**
    (confirmed by diff: restoring the 3 files to main brought it back to 1 second). Not visible in `make test`; it surfaced as a timeout
    in CI's release full roast.
  - **Lessons / design constraints for the next slice**: sibling-closure sharing must narrow the boxing targets via **escape analysis (box only
    the captures of closures that escape = are returned/stored externally. Do not box immediately-invoked argument closures like `lives-ok {…}`)**, and
    must first **make mutation-writeback cell-aware**. The naive "just drop the loop-only restriction" is not viable. This is also empirical proof of
    §6 "guaranteeing speed = omit containers via escape analysis".
  - After the revert, main is clean (keeping only the `into_deref` groundwork of `#2742`). `int.t` is back to 1 second.
- 2026-06-08: **Phase 1 slice 1 redesign (escape-aware) = success**. Per the previous lessons, narrowed the boxing targets with a precise signal and
  implemented container sharing for non-loop sibling closures.
  - **Compiler signal `multi_captured_mutated_locals`** (`opcode.rs` `compute_free_vars`): counts the distinct child closures capturing
    an own-local, and collects those with **≥2** and `captured_mutated` into a set.
  - **`box_captured_lexicals`** (`vm_register_ops.rs`): boxing condition becomes (A) loop locals (existing, unchanged) OR
    (B) `multi_captured_mutated_locals` (new; non-loop siblings). The type/`where` constraint guard applies only to (B)
    (the loop path is kept byte-for-byte). The `>=2` restriction excludes `lives-ok {…}` (1 closure), **structurally avoiding** the previous
    perf explosion (`Arc<Mutex>`+env COW per closure creation) and the correctness regression (the test-helper pattern).
  - **Made mutation-writeback cell-aware**: added `ContainerRef` arms to `overwrite_instance_recursive` (env path) and
    `overwrite_instance_in_locals` (locals path) (a boxed scalar holding an instance that receives a mutating method now propagates
    through the shared cell). Added `into_deref` to the direct Var read in `try_eval_simple_protect_expr` (preventing value leaks in the fast path).
  - Verification: target `make(); $s(42); $g()` → `42`. **`int.t` stays ~1s/165 (no perf regression)**; an 11-file heavyweight roast
    sample of 2314 tests completes in 3 seconds. `where-constraint-var.t` (block/whatever where protected by the guard) /
    `submethods.t` / `S24-testing` / the closure suite all PASS. `make test` PASS (456 unit + 5315 prove).
    `gather.t` 38 is the known Phase 2 take-rw gap (non-whitelist, unrelated).
  - **Out of scope (next slice)**: a single escaping closure (`my &f; { my $a=3; &f=sub{$a++} }` → 3,0) has 1 closure, does not enter multi, and
    is unfixed. `.VAR.^name` reflection / `is rw` 3-way persistence are also separate slices.
- 2026-06-08: **step 1 = escape analysis (compiler, keystone) = success**. PLAN.md §🟣 "Implementation order" step 1.
  Replaced `multi_captured_mutated_locals` (the proxy "captured by ≥2 sibling closures") with the true signal, **escape analysis**
  (does the value of the capturing child closure escape the frame?). Being the real mechanism rather than a proxy, it **structurally** catches the single
  escaping closure while keeping immediate invocations (`lives-ok {...}`/`map {...}`) unboxed.
  - **Compiler signal**: `Compiler::escaping_position` (bool) + the `with_escape(escaping, f)` save/restore helper.
    escaping=true: assignment/`:=` RHS (`compile_assignment_rhs_for_target`), `return`/`fail` operands, block/
    routine tails (3 last-`Stmt::Expr` sites in `helpers_sub_body.rs` + the top-level tail in `mod.rs`), array/hash/
    capture literal elements. escaping=false (default): call arguments (reset in `compile_call_arg`/`compile_method_arg`
    = the #2746 guard; even when the call is in an escaping position like `my @r = map {...}`, argument closures stay unboxed).
  - **Data**: `CompiledCode.closure_escapes: Vec<bool>` (index-aligned with `closure_compiled_codes`).
    `add_closure_code(code, escapes)` records `escaping_position`. `compute_free_vars` enumerates children, checks
    `closure_escapes[i]`, and if a captured×mutated own-local is **captured by an escaping child**, adds it to `needs_cell_locals`
    (renamed from the old `multi_captured_mutated_locals`). `captured_mutated_locals` (for path A loop boxing) is unchanged.
  - **VM**: path B of `box_captured_lexicals` references `needs_cell_locals` (the type/`where` guard and path A are byte-for-byte unchanged).
  - Verification: single escape `$f = sub{$a++}` (`my $f`/`&f()`) → 3/4 (was 3/0). Siblings `make(); $s(42); $g()` → 42 (subsumed).
    Factory `return sub{$n++}` → 0/1/0. Immediate invocations `for`/`map` → unboxed (3 / 12). **Perf canary
    int.t stays 0.21s (no #2746 regression)**. `make test` PASS (458 unit + 5315 prove), clippy clean,
    S03-binding/closure, S04-declarations/state, pointy(-rw) etc. whitelist PASS.
  - **Also fixed the bareword `f()` escaped-block-capture bug**: `my &f; { my $a=3; &f=sub{$a++} }; f(); f()` yielded
    `3,0` (read-only `sub{$a}` gave `3,Nil`). Bareword `f()` goes through the interpreter's `call_sub_value`, while
    `&f()`/`$f()` go through the VM's cell-aware dispatch (`call_compiled_closure`) — under the dual-store split the VM path was already correct.
    Cause: `call_sub_value`'s captured-env merge used `merge_all=true` and applied `entry_or_insert` (caller wins) even to
    `ContainerRef` (shared cells), so a stale value leaked from the declaring block, or the previous call's writeback, hid the
    live cell, and it was lost on the 2nd call. Fix: **in the merge, only `ContainerRef` uses `insert_sym` (overwrite) so the
    live cell is always adopted** (the same "the cell is the source of truth" principle as the VM's `call_compiled_closure`; since `box_captured_lexicals`
    boxes captured×mutated scalars, `$a` is a cell in both the read-only and mutating cases). **Dynamic variables and non-cell values are untouched**, so
    `note`/`$*ERR` rebind (`note-gist-and-dynamic-handle.t`) does not regress.
    - **Tried and reverted (important lesson)**: initially tried the broader version "force `insert_sym` for the closure's actual scalar lexical free vars too",
      but it regressed `S17-scheduler/{at,in,every}.t` and `S32-io/IO-Socket-Async.t` (force-overwriting the internal closure scalars of `:in`/`:at`
      delayed scheduling made callbacks over-fire; every.t "seen 38 runs"). Not visible in `make test` or isolated repros —
      **settled by the release-roast comparison: main 5/5 PASS vs branch 5/5 FAIL**. Shrunk to the minimal `ContainerRef`-only version, which solved it
      (the double-count regressions of `trans.t`/`squish.t` disappeared at the same time). Lesson: the call_sub_value merge is the hot path for all
      interpreter closure calls, and forcing scalar values interferes broadly with internal closures like the scheduler's. **Touch only `ContainerRef`, which is already shared.**
    `make test` PASS (458 unit + 5336 prove), clippy clean, int.t 0.20s, the 4 regression files release 8/8 PASS.
  - **Next (step 2)**: cell-ify needs-cell `$` locals at declaration time and delete the boxing heuristics of
    `owned_captures`/`closure_captured_state`/`box_captured_lexicals` (4→1) (PLAN.md implementation order step 2).
