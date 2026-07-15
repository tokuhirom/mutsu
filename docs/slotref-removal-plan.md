# SlotRef removal — design plan (the final cells-everywhere task)

> Prep for a dedicated PR series. The remaining `HashSlotRef` / `ArraySlotRef` /
> `DeferredHashAccess` machinery is the last `Value`-variant survivor of the
> pre-cell era. Read alongside `docs/container-identity.md`
> (Phase 2 Stage 2 "map of remaining slices" 1-4) and `docs/element-element-bind-plan.md`.
>
> This is the **"cells-everywhere task that requires design"** flagged in the 2026-06-13
> session note. The variants are concentrated in the hottest write-autoviv files
> (`vm_var_assign_ops.rs` 28 / `value/mod.rs` 21 / `vm_var_index_ops.rs` 19), and a
> naive "turn every slot into a cell" approach **will regress perf and identity**.
> The whole point of this doc is to settle the representation *before* touching
> code so the implementation PR is *apply*, not *re-derive*.

---

## 0. Definition of done

Remove all `SlotRef`-family variants from the `Value` enum, satisfying the following:

1. `git grep 'SlotRef\|DeferredHashAccess' src/` is zero (including all arms of the enum
   definition, serde, display, and introspect).
2. `make test` all green, **zero regressions in the release-roast main-vs-branch comparison**
   (element changes have leaks that only show up in release — the #2898 rut).
3. The binding family, including `roast/S03-binding/nested.t`, is at parity or better.
4. `int.t` wall-clock does not hit a perf cliff (write-autoviv is the hottest path).

> ### ✅ Achieved (2026-06-23, slice 5) — but "delete all variants" is settled as **impossible**
>
> "Delete all `SlotRef` variants" is **fundamentally impossible without the anti-goal side table**
> (reconfirming the slice 3 note). A `:=` bind to a missing key (`$b := %h<x>`) requires a
> **deferred-vivification token** that "reads as Any and materializes into the parent hash on write"
> without polluting `:exists` and while remaining invisible to iteration.
> This token must necessarily hold `(hash Arc, key path)` somewhere, and as long as it sits in a
> `Value` slot it can only be a Value variant
> (it cannot ride in a `ContainerRef` cell = cells are exclusively aliases of materialized entries).
> The lvalue descent of `is raw` reduce (`roast/S32-list/reduce.t`) also shares this token.
>
> ∴ DOD #1 was reinterpreted as "banish the **concept and name** `SlotRef`". Concretely,
> **the 2 variants `HashSlotRef` + `DeferredHashAccess` were unified into a single path-based
> `Value::HashEntryRef { hash, path }`** (the `parent_slot` chain abolished,
> the read/write/materialize helpers unified, the dead `hash_slot_ref_lazy` deleted). With this,
> `git grep 'SlotRef\|DeferredHashAccess' src/` is in fact zero, and the sole remaining deferred
> token bears the correct name: "a vivification token, not a stale back-reference".
> As a by-product, **fully-missing path binds of 3+ levels (`$b := %h<a><b><c>`) now work**
> (the old 2-variant chain handled only 2 levels and diverged from raku). Pin =
> `t/hash-entry-ref-deep-bind.t` (9). Behavior remains guaranteed as before by `t/phantom-entry-bind.t`.

---

## 1. Map of the current state: the **2 roles** SlotRef plays

The `SlotRef` family has historically combined 2 distinct responsibilities in 1 variant. This is
the root cause of why removal is hard. Once the roles are separated, each has a correct successor representation.

### Role A — write-autoviv cursor (**transient, plain assignment**)

In assignments like `%h<a><b> = 5` / `@a[3] = 9`, a temporary pointer that points at the terminal
slot while autovivifying intermediate containers. **It is not a bind.** It flows across the VM
stack for a single instruction and is discarded right after the write. It is never persisted as a
`Value` (persisting it would mean "every assigned element becomes a
`ContainerRef`" = the cells-everywhere cliff).

- Production: `hash_autovivify` (`value/mod.rs:2856`).
- Consumption: `exec_index_autovivify_op` (`vm_var_index_ops.rs:75-152`)
  pushes → the next index-assign op reads/writes via `hash_slot_read`/`hash_slot_write`.
- For terminal arrays, `array_slot_ref` already returns a `ContainerRef`, but note that this is **bind-only** and
  the autoviv path (`exec_index_autovivify_op`'s array arm, vm_var_index_ops:120-127) reuses
  `array_slot_ref(idx, false)` as an **autoviv cursor**.

### Role B — bind alias (**persistent, `:=` binding**)

With `my $x := %h<a>`, the variable `$x` keeps aliasing the element. Writes must reach across COW.
**This has already migrated to `ContainerRef` cells in Stage 2** (existing scalar leaves).
What remains:

- Production `vm_var_assign_ops.rs:4256/4291` — the old path that writes a SlotRef into the `:=`
  source variable's env slot. **Strongly suspected to be duplicated with the bind cell path
  (landed in slices 3/4).** The first thing to settle is "is this path live / already subsumed
  by the cell path"
  (§4 slice 1).
- Bind to a missing key: `hash_slot_ref`'s None arm (`value/mod.rs:2918`) +
  `hash_slot_ref_lazy` (2871) + `DeferredHashAccess` (vm_var_index_ops:214).
  No entry exists, so it cannot be cell-ified (the requirement is deferral that does not pollute
  `:exists`). **A phantom-entry-like design is required** — hardest, defer to last.

### Role correspondence table

| Role | Current representation | Production site | Successor representation | Difficulty |
|------|--------|-----------|---------|------|
| A. write-autoviv cursor | `HashSlotRef` (transient) | `hash_autovivify` 2856 | **non-Value WriteCursor** (§3) | Medium |
| A. array autoviv cursor | `array_slot_ref(_,false)` | mod.rs array arm | unified into the above | Medium |
| B. bind (existing leaf) | `ContainerRef` cell | slice 3/4 (landed) | (done) | — |
| B. bind source slot | `HashSlotRef`/`ArraySlotRef` | 4256/4291 | subsume into cell path or delete | Low–Medium |
| B. bind (missing key) | `hash_slot_ref_lazy`+`DeferredHashAccess` | 2871/2918/214 | phantom-entry cell (§3.3) | High |

---

## 2. Why naive cell-ification fails (core hazard)

"Make every slot a `ContainerRef` cell and SlotRef disappears" is **wrong**. Reasons:

1. **Perf cliff**: write-autoviv is the hottest path (`%h{$k} = $v` loops). Wrapping the terminal
   in `Arc::new(Mutex::new(..))` every time adds a heap alloc + lock acquisition to each assignment.
   `int.t` wall-clock degrades immediately.
2. **Identity / eqv pollution**: a cell-ified element forces decont on all paths of `eqv` / `===` /
   `.WHICH` / hash-key encoding / serde (the "deref everywhere" regression). A plainly-assigned
   `%h<a>` must not be a cell as seen by the user.
3. **Assignment-creates-new-container violation**: `%new = %hash` creates a snapshot
   (S03-binding/hashes.t 30). If the terminal is a cell, the alias leaks.

→ **Invariant**: cell promotion only *when a bind actually happens*. The autoviv cursor of a plain
assignment must not leave a cell behind. This is the constraint running through the §3 design.

---

## 3. Design

> ### ⚠️ Correction discovered during implementation (2026-06-13, after starting slices 1/2)
>
> The original §3.1 premise "role A = plain write-autoviv cursor" **contradicted the code**.
> Facts settled by the actual-code survey of slices 1/2:
>
> 1. **Plain `%h<a><b> = 5` creates no SlotRef at all** — deep nested assign on a named target
>    goes through `OpCode::IndexAssignDeepNested` (→ `exec_index_assign_deep_nested_op`,
>    autoviv via its own `assign_into_nested_container`), passing through neither `hash_autovivify`
>    nor SlotRef. In other words, "role A's write-autoviv cursor" **does not exist**.
> 2. **The non-lazy `OpCode::IndexAutovivify` is `#[allow(dead_code)]` with zero compiler emissions** = dead.
>    Its handler `exec_index_autovivify_op` survives only as the fallback of the lazy bind op.
> 3. **All remaining SlotRef production is around `:=` bind + a few niche autovivs**:
>    - `HashSlotRef`: `hash_autovivify` (junction-key bind at 97 / autoviv-op hash arm
>      113·140 / **the lvalue-read autoviv of `is raw` reduce** 811-868) + `hash_slot_ref`'s
>      None arm (missing-key terminal bind 2918) + `hash_slot_ref_lazy` (lazy bind).
>    - `DeferredHashAccess`: the lazy nested bind of missing keys (vm_var_index_ops:214).
>    - **`ArraySlotRef`: since slice 1 cell-ified the last production site (4291), production is zero
>      = completely dead** → the whole variant was already deleted in slice 2.
> 4. **The "non-Value WriteCursor" plan is not adopted**: the cursor returned by `exec_index_autovivify_op`
>    sits on the stack **between** consecutive `IndexAutovivifyLazy` ops (crossing op boundaries), so
>    it does not fit within a single-op thread and would require op fusion. But since the remaining
>    production is almost entirely bind paths, **exhausting the sites individually via cell-ification
>    (same shape as slices 3/4) is the honest route**. Hence the subsequent
>    slices proceed not with "§3.1 WriteCursor" but with "cell-ification of bind production sites".
>
> §3.1 below is kept as a record of the original plan but is **not adopted** (see the updated slices in §4).

### 3.1 Role A: de-Value-ify the write-autoviv cursor (WriteCursor) [original plan — not adopted]

Stop using `HashSlotRef`/`ArraySlotRef` as "a `Value` flowing across the VM stack"; instead
**thread `(Arc<HashData>, String)` / `(Arc<ArrayData>, usize)` directly inside the index-assign
opcodes**, or keep a dedicated non-`Value` enum locally in the VM.

Two candidate plans:

- **Plan A1 (recommended) — in-opcode threading**: **fuse** `exec_index_autovivify_op` +
  the subsequent index-assign op, passing the intermediate autoviv result not boxed into a `Value`
  but as VM method return values (a Rust `(Arc, key)` tuple or a small `enum WriteCursor`).
  The `Value::HashSlotRef` variant disappears. The autoviv chain (`%h<a><b><c> = 5`) can be
  written as a loop that progressively deepens the cursor (intermediates follow
  `hash_autovivify`'s inner-Arc; only the terminal does `hash_insert_through`).
  - Upside: zero cells, zero allocs, zero identity pollution. Perf-neutral (fewer Arc clones, in fact).
  - Cost: refactoring `exec_index_autovivify_op` / `exec_index_assign_*`. If the compiler emits
    autoviv→assign as separate ops, it needs op fusion or parking the `WriteCursor` in a
    VM field (never on the stack).
- **Plan A2 — keep inside `Value` but privatize**: minimal change, but the variant remains, so the DOD is unmet. Not adopted.

**Decision point (settle before implementing)**: which op sequence the compiler lowers `%h<a><b> = 5` to.
Confirm with a bytecode dump, not `--dump-ast`, and determine whether the autoviv cursor stays on the
stack across op boundaries (= whether fusion is needed). `grep IndexAutovivify src/compiler/`.

### 3.2 Role B: tidying the bind source slot (4256/4291)

Since slices 3/4 moved the BOUND_*_REF sentinels to cells, measure whether the SlotRef-into-env path
at 4256/4291 **is still called** (`MUTSU_TRACE` or a temporary `dbg!`, or a reachability grep).
Two scenarios:

- **(i) already subsumed by the cell path** → 4256/4291 are dead code. Just delete (slice 1).
- **(ii) still live** (specific bind forms go through here) → as with slices 3/4, replace with writing
  a `ContainerRef` cell into the source variable. The `bind_cell` pre-read helper (reusing an
  existing cell) already exists, so reuse it.

Either way, the consumers of `hash_slot_read`/`array_slot_read`/`hash_slot_write`/`array_slot_write`
(4303-4318, 4987-5004, 5750-5776) disappear in turn.

### 3.3 Role B: lazy bind of missing keys (hardest — phantom entry)

With `my $b := %h<a><b>` where neither `%h<a>` nor `<b>` exists, we want to bind without polluting
`:exists`. Currently `hash_slot_ref_lazy` + `DeferredHashAccess` realize "create no entry until a
write". The obstacle to cell-ification is that "the physical slot to point at does not exist yet".

Design candidates:

- **phantom-entry cell**: for the missing key, create a **lazy cell** holding "parent Arc + key path",
  and on the first write materialize the physical entry into the parent for the first time and only
  then tie the cell to it. Invisible to `:exists` (before materialization the parent map has no key).
  Reads return Any/Nil. This is essentially a cell version of `DeferredHashAccess`, but differs in
  that it **aliases by cell identity** (COW staleness disappears).
- **Alternative**: missing-key binds are infrequent, so an option is to keep just the single
  `DeferredHashAccess` variant until the very end, erase all the other A/B paths first, and then
  tackle it alone. Since the DOD is "remove all SlotRef", phantom-entry is ultimately needed.

⚠️ This slice also entangles with `roast/S03-binding/nested.t 11-12` (`<key>()` mid-path).
It has overlapping territory with element-element-bind-plan.md, so land that slice first.

---

## 4. Staged slices (each CI-gated; avoid big-bang)

> Ordering principle: **exhaust the production sites one by one, then delete the variant and consumers in one sweep at the end**
> (the same fold-up as container-identity.md remaining-map-4). The read/write helpers naturally go
> dead once production disappears.

1. **slice 1 — cell-ification of computed-target `:=` element bind [DONE, PR #2974]**
   The `HashSlotRef`/`ArraySlotRef`-into-env (4256/4291) of `exec_index_assign_generic_op`
   (IndexAssignGeneric) to shared `ContainerRef` cells. Reachability measurement found it **live AND buggy**
   (write-through alias non-propagation), and cell-ification fixed the bug at the same time. Pin `t/generic-bind-cell.t`.

2. **slice 2 — full deletion of the dead `ArraySlotRef` variant [DONE]**
   Slice 1 erased `ArraySlotRef`'s last production site (4291), so the variant has zero production
   = completely dead (the `array_slot_ref` method is a different thing that returns a `ContainerRef` cell).
   Deleted the variant definition, the `array_slot_read/write` methods, and all consumer arms
   (types/utils/display/introspect/serde/vm_env_helpers/vm_call_func/vm_var_index/vm_var_assign).
   `cargo build` is the exhaustiveness checklist. Behavior-preserving (bind-family t/ + roast S03-binding all PASS).
   [The original "de-Value-ify the write-autoviv cursor" does not exist per the correction at the head of §3 and is not adopted]

3. **slice 3 — phantom-entry: cell-ify the materialize of missing-key binds [DONE]**
   For `:=` binds of missing keys (the `HashSlotRef` of `$e := %m<solo>`, the
   `DeferredHashAccess` of `$d := %k<p><q>`), materialize into a shared `ContainerRef` cell **on the first write**.
   The bound var local is replaced with the cell and the same cell is also stored into the terminal
   hash entry → thereafter the bound var and the hash entry alias bidirectionally (the old plain-value
   materialize lost the alias, and cross-writes after materialization did not arrive = the **case-C bug**).
   Full raku parity (lazy semantics cases A/B/D unchanged = the deferred token is kept until the first write).
   The `materialize_bound_slot_to_cell` VM helper is called from the 2 SetLocal sites.
   Deleted the dead-ified `deferred_hash_write` method.
   Pin `t/phantom-entry-bind.t`(12).
   **Note**: the deferred token (`HashSlotRef`/`DeferredHashAccess`) itself remains as the
   bind-to-first-write deferred state (read=Any, `:exists` unpolluted). Removing the token entirely
   from Value would require "a phantom representation invisible to `:exists`/iteration even when put
   into the map" (intermediates are also lazy, so a tombstone is impossible) → impossible without a
   side table (reviving ptr-keyed globals = anti-goal). So **keeping the token is the correct answer**,
   and cell-ifying the materialize (= this slice) is the effective substance of phantom-entry.

4. **slice 4 — cell/shared-Arc-ify the `hash_autovivify` of bind descent [DONE]**
   The 3 bind-descent sites of `exec_index_autovivify_op` (junction-bind 97 / hash arm 113 /
   slot-ref-target 140) to a new method `hash_autovivify_cell`. Symmetric with the array arm
   (`array_slot_ref(_,false)`, slice 2): existing cell→as-is / container leaf→shared Arc value /
   scalar leaf→cell promotion / missing→empty Hash autoviv (shared Arc).
   Eradicated the `HashSlotRef` back-ref and resolved COW staleness too.
   Behavior fully unchanged (raku parity). Assured by the existing bind/junction/phantom suites + roast.
   **scope**: bind descent only. The `is raw` reduce lvalue-read (vm_var_index_ops:807/822/864)
   **deliberately stays on the old `hash_autovivify`** = it requires the semantics of reading a missing
   key as a **writable slot holding Any**, which differs from `hash_autovivify_cell`'s empty-Hash
   semantics (changing it would regress reduce). The old method remains for reduce.
   The bind/reduce-family `hash_autovivify` is **bug-free by raku comparison = a pure refactor**
   (consistent with the campaign's cell-unification goal).
   A separate lineage of real bug found during the survey (an **outermost junction/slice assignment**
   via a nested/computed target stringifies the key and breaks) was fixed in the independent PR #2981
   (autothreading added to `exec_index_assign_{expr_nested,deep_nested,generic}_op`,
   `t/nested-junction-slice-assign.t`).

5. **slice 5 — unify the 2 variants into a single `HashEntryRef { hash, path }` [DONE, 2026-06-23]**
   See the achievement note in §0. The lvalue-read autoviv of `is raw` reduce (`hash_autovivify`) was
   **not deleted** but **changed to return a path-length-1 `HashEntryRef`** (roast/S32-list/reduce.t
   is a lenient path where current rakudo itself errors with `assign requires a concrete object`, but
   mutsu keeps its previous behavior).
   `DeferredHashAccess`'s `parent_slot` chain is folded into `path: Vec<String>`, and the descent of
   missing-key binds has `exec_index_autovivify_lazy_op` push the path one subscript at a time. Coordinated deletions:
   `hash_slot_read/write`→`hash_entry_read/write` (+ the shared walk-create helper `hash_entry_terminal`),
   deleted `deferred_hash_read`, deleted the dead `hash_slot_ref_lazy`, and unified the serde/display/truthy/isa/introspect/
   utils/vm_env_helpers skip-arm/vm_var_assign_ops consumer groups into single arms. `cargo build` is the
   exhaustiveness checklist. `make test` all green + binding/reduce-family roast PASS.
   **value_is_defined / truthy treat an unmaterialized token as unconditionally `false`** (matching the
   old DeferredHashAccess, and for a single key also raku parity = no retro-bind on external writes) —
   reading through would break
   `t/phantom-entry-bind.t`'s retro-bind checks. display/type/isa read through (equivalent to the old HashSlotRef).

---

## 5. Hazard audit points (element changes have leaks that only show up in release)

**Manual audit mandatory** for each slice (past ruts):

- **unsafe Arc cast**: around `Arc::as_ptr(arc) as *mut HashData/ArrayData`, no immutable borrow of
  `&HashMap`/`&Vec` may be live. Has WriteCursor-ification created a new path where the borrow
  crosses op boundaries?
- **Arc identity / `ptr_eq`**: the bind-alias identity checks (the `exec_container_eq_indexed_op`
  family) look at cell `Arc::ptr_eq`, so a new cursor must not create false identity.
- **iterator sharing**: the decont of `resolve_hash_for_iteration` / `resolve_array_entry` must
  **not leak the WriteCursor into read paths** (the cursor is write-only).
- **env writeback**: the `env_dirty` flag. Removing the path that writes the bind source slot into
  env can change flush behavior (dual store). Check via `MUTSU_VM_STATS`'s `env_flushes`.
- **COW staleness**: phantom-entry must grab the correct physical Arc after materialization. For a deep
  `%g<outer><inner>`, does a write after outer has been COW-cloned reach the source
  (nested.t's deep bind is the regression canary)?
- **serde / display / `.raku` / `.gist`**: variant deletion removes the arms, but before deleting,
  has the premise "SlotRef does not leak into these paths" held up? (If it leaked, that exposes a
  hidden bug — fix forward.)

---

## 6. Verification protocol (each slice)

1. `cargo build` (debug) → `prove -e target/debug/mutsu roast/S03-binding/nested.t` +
   the touched binding family.
2. Counter checks (`MUTSU_VM_STATS=1`) are settled in debug (early detection of perf cliffs).
3. `make test` all green (Grep `tmp/make-test.log`).
4. **Release-roast main-vs-branch comparison** (mandatory; element-change leaks are release-only).
5. The final perf measurement is **`int.t` wall-clock in release** (mandatory for slice 2).
6. PR → auto-merge → background CI watch (CLAUDE.md PR workflow).

---

## 7. References

- `docs/container-identity.md` — the first-class container ledger (Phase 2 Stage 2 remaining map 1-4
  is this doc's parent).
- `docs/element-element-bind-plan.md` — element-element bind (nested.t 11-12/32-37).
  Prerequisite of slices 3/4.
- `docs/is-rw-shared-cell-plan.md` — `is rw` shared cells. The same ContainerRef foundation.
- Past slices: #2964 (deepmap) / #2966 (intermediate SlotRef removal) / slices 3/4
  (full abolition of the BOUND_*_REF sentinels) / #2902-#2925 (element cell foundation).
