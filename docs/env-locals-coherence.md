# env↔locals container coherence — the true core of Slice F (design)

> **Status:** DESIGN (2026-06-18). No code. Following the 2026-06-18 conclusion that
> `docs/vm-single-store.md` (dual-store unification) and
> `docs/container-identity.md` (first-class containers) **converge at Slice F**,
> this document re-examines from the design level how to guarantee that convergence point —
> "`env` and `locals` must not diverge on the same container".
> Slice E (closure upvalues, #3245/#3247) settled the *independent* prerequisites of single-store,
> so this coherence is the only thing left.

---

## 0. TL;DR

The **sole remaining prerequisite** of Slice F (deleting `env_dirty` / `sync_locals_from_env` /
`ensure_locals_synced` / `saved_env_dirty` = making `locals` the true single authority) is:

> **For the same logical container (an Array/Hash including `:=` cells), `env` and `locals`
> must never hold *different outer `Arc`s* and diverge structurally.**

The `ContainerRef` cell of the first-class container campaign (`docs/container-identity.md` Phase 2)
already solves leaf survival across COW **within one store**, but **the dual-store env↔locals divergence
remains as a separate layer**. This is the root cause of the `pairs`/`slip` carrier-drop breaking
`t/element-bind-cell.t` (it persists even with the cell mechanism).
This document records an exact map of the divergence, why cells alone do not close it, the design options,
staged slices, and hazards.

---

## 1. The two stores, and the mechanism that *normally* keeps containers coherent

| store | shape | role |
|---|---|---|
| `Interpreter::locals` | `Vec<Value>`, slot index | hot path, `GetLocal`/`SetLocal` |
| `Interpreter::env` | `Env` = `Arc<HashMap<Symbol,Value>>` (COW, scoped-overlay chain) | all consumers keyed by name |

Sync primitives (`src/vm/vm_env_helpers.rs`):
- **forward** (locals→env) = `flush_local_to_env(code, idx)`: `set_env_with_main_alias(name, self.locals[idx].clone())`.
  For Array/Hash, `Value::clone` is an **`Arc` bump = shares the outer Arc**. Gated by `needs_env_sync[idx]` plus
  `simple_locals||bare_param`.
- **reverse** (env→locals) = `sync_locals_from_env(code)`: for each local name, `self.locals[i] = env.get(name).clone()`
  (likewise an Arc bump = shared). `HashSlotRef` / `!attr` are skipped. Called from `ensure_locals_synced`,
  gated by `env_dirty`.

**∴ Normally, a container that has a slot shares the *same outer `Arc`* between env and locals** (every clone is an Arc bump).
The moments this sharing breaks — i.e., divergence occurs — are §2 below.

---

## 2. When does divergence happen (the moment the outer `Arc` becomes different between env and locals)

`Arc::make_mut` (`Env::cow_mut` / in-place mutation of container elements) **deep-copies when strong_count>1**
and creates a **new Arc**. This is the source of divergence:

1. **Only one store runs make_mut**: mutating a container on the `env` side via `env_mut()` (a carrier's
   by-name write, an insert into a scoped overlay) duplicates env's outer Arc, making it different from locals'.
   The same happens if the locals side is mutated directly. If the next reverse pull (`sync_locals_from_env`)
   runs, locals re-fetches env's new Arc and **re-converges** —
   that is the job of `env_dirty`. **Dropping `env_dirty` = skipping this re-convergence** = the divergence remains.
2. **A carrier builds a fresh env**: an EVAL/pairs/slip carrier runs on a scoped overlay or flatten, and
   containers can be COW-detached. The carrier-return `writeback_carrier_writes` reconciles **scalars only** and
   **never overwrites containers** (because a COW-detached env would break internal `:=` cells — the
   S03-binding/nested.t hazard, #3227).
   In other words, containers are delegated to the barrier pull (env_dirty). Dropping it leaves container divergence.
3. **scoped_child / flattened**: when deep recursion pushes the chain past `MAX_OVERLAY_DEPTH`, the parent is
   flattened (`env.rs`). The flatten itself is an Arc bump, but if a subsequent overlay write triggers make_mut,
   the same applies as above.

**Leaf cells do not diverge**: `ContainerRef(Arc<Mutex<Value>>)` keeps the **`Arc<Mutex>` shared between clones**
(Arc bump) even under COW deep-copy, so a leaf cell's identity survives across paths of arbitrary depth
(the core of container-identity Phase 2).
**What diverges is the *structure* of the outer container** — "which cell lives at which index, the length, the non-cell elements".

---

## 3. Why cells (Phase 2) alone do not close it — concrete case

Deep bind in `t/element-bind-cell.t` (tests 39-44):

```raku
my $struct = [ "ignored", { key => { subkey => [10, 42] } } ];
my $abbrev := $struct[1]<key><subkey>[1];   # promotes the leaf element to a cell; $abbrev is the same cell
$struct[1]<key><subkey>[1] = 43;            # element write → cell → $abbrev is also 43 (OK across COW)
$abbrev = 44;                                # cell write → $struct's leaf is also 44
```

This works via cell sharing **within a single store** (#2922/#2925, nested.t 43/43).
But when the `pairs`/`slip` carrier drops `env_dirty`, it breaks (memory sessions 13/14, tests 9/28/41/44/46/47):

- Inside the carrier, `$abbrev = 44` (a cell write) **reaches the leaf cell** (the cell is shared; it does not diverge).
- But if the `env`-side `$struct` outer Arc gets COW-detached during the carrier, then at carrier-return the
  **container is not written back** (§2-2), and with the drop the **barrier pull is also skipped**, so the
  `locals`-side `$struct` keeps reading the **old outer Arc** (which has the cell at a different index / a different structure).
- As a result, reading `$struct[1]<key><subkey>[1]` via locals traverses a **stale path** and yields `Nil` / the old value
  (the leaf cell is alive, but the outer structure leading to it differs between env and locals).

**∴ Cells guarantee leaf identity, but they do not guarantee *outer structural identity* between env and locals.**
That is what "a separate layer" means, and Slice F = guaranteeing this outer identity.

---

## 4. Design options

### (A) Hold containers as cells (`ContainerRef`) in env too = **env↔locals share the outer cell** (the recommended axis)

Same shape as Phase 3 unifying instance attrs onto a single `Arc<RwLock<HashMap>>` cell. Wrap slot-holding
Array/Hash in a **`ContainerRef` cell, and have env and locals hold *the same cell***. Then:
- Both forward and reverse become Arc bumps of the cell = the outer structure is always shared too (even if one
  side detaches via make_mut, both point at the same cell, so the detach itself does not happen — or if it does,
  it is confined to one place inside the cell).
- The **re-convergence** of `env_dirty`/`sync_locals_from_env` becomes unnecessary (divergence structurally cannot occur)
  → Slice F is unblocked.
- Carrier-drop is also made safe (containers are cell-shared, so they no longer depend on the pull)
  → the `pairs`/`slip` generalization lands at the same time.

**Costs/hazards**:
- **Perf-cliff risk**: if all containers are cell-ified, the consumption surface of value ops (arithmetic folds,
  iteration, native methods' raw-items reads) has to decont a ContainerRef every time = "deref everywhere".
  **Escape-awareness is mandatory** (bare locals that are not captured/aliased/`:=`-bound/
  `is rw`/`.VAR`-taken are NOT cell-ified — do not repeat the #2746 mistake). The single decont chokepoint of
  Phase 2 (`docs/container-identity.md` §3) is a prerequisite.
- **Leak hardening**: audit that cells do not leak via slice/`.kv`/`.pairs`/`.raku`/native methods' raw-items reads
  (a known Phase 2 issue).
- **Write chokepoints**: array/hash element writes are already funneled through `assign_element_slot`/`hash_insert_through`
  (Phase 2 Stage 0). Outer cell-ification builds on top of that.

### (B) Coherence invariant = "never make_mut a slot container's outer Arc on only one side"

An invariant that, so env and locals always point at the same Arc, **immediately before mutating either one you
must relink the other to the same Arc**. Restrict forward/reverse to "write-throughs that never break Arc identity".
A **weaker version of (A)**, but since make_mut always detaches at strong_count>1, completely preventing
"mutation on only one side" is hard, and it ultimately converges on cell unification (A).
Kept for the record, but not the main line.

### (C) Carriers precisely log container writes too = make carrier-drop safe (local symptomatic fix)

Have the `pairs`/`slip` carrier record container mutations in `carrier_writes` as well, and do a cell-aware
reconcile at carrier-return. **Proven in memory session 14 to break deep `:=` cell coherence** (even with complete
write-logging, the outer divergence remains).
= A symptomatic treatment that does not solve the root (outer identity). **Rejected** (recorded only).

---

## 5. Recommendation = introduce (A) in stages, on top of a completed Phase 2

> North star (PLAN.md §🟣) = cleanest × fastest. The progress metric is **the number of deleted
> duplicate/special-case mechanisms**.
> (A) is the winning line that sweeps away the 4 `env_dirty`-family mechanisms + the carrier container-reconcile
> special case + the pairs/slip carrier-drop suppression.

A big bang is not possible. Just as Phase 2 (element cells) solved leaves, **outer container cell-ification
also proceeds in the order "chokepoints first (behavior-preserving) → escape-aware promotion → mechanism deletion"**:

- **Stage 0 — inventory of outer read/write chokepoints (behavior-preserving)**: confirm and complete that
  reads of slot containers (GetLocal/GetGlobal/GetArrayVar/GetHashVar/Index) go through a single decont and
  writes go through a single write-through. An extension of Phase 0/2 Stage 0. Acceptance criterion: exact roast parity.
  - **[~] Inventory result (2026-06-18)**: **the read side is already funneled through a single decont chokepoint**
    = better than expected. `GetLocal`/`GetGlobal`/`GetArrayVar`/`GetHashVar` all strip a top-level
    `ContainerRef` via `into_deref()` (`value/mod.rs`) before pushing (`vm_var_assign_ops.rs:5388`,
    `vm.rs:1347/1439/1521`), and element reads decont cells in
    `resolve_array_entry`/`resolve_hash_entry` (`vm_var_ops.rs`). **The value-consumption surface of outer cells
    (open-q#1) is already closed at the slot-read boundary** = the main risk of Stage 1 is smaller than expected.
    On the write side, element writes are already funneled into
    `assign_element_slot`/`hash_insert_through` (`value/mod.rs`), and for whole-container write-through,
    `SetLocal` already preserves an existing `ContainerRef` (`vm_var_assign_ops.rs:5543`).
  - **[x] Filled 1 write-chokepoint gap — spurious readonly firing on `our %g := %h` (global hash bind)**:
    an `our` `:=` binding marks the var readonly (a bind signal), but `SetGlobal` misread that as genuinely RO and
    aborted the bind itself with "Cannot assign to a readonly variable (%g)" (`my %g := %h` worked; specific to
    global hashes; array globals use a different mechanism and were unaffected). Fix = re-emit `MarkBindContext`
    right before the `SetGlobal` of a bound-container vardecl (`compiler/stmt.rs`) + have `SetGlobal` skip the
    readonly check while in bind context (`vm.rs`). After the bind the var keeps its readonly mark = subsequent
    `%g<k>=v` mutates in place via the named index-assign `is_bound_hash_var` path, establishing the bidirectional
    alias (same shape as locals). `t/our-hash-bind.t` (9).
  - **[~] Remaining Stage 0 gaps flushed out by differential probing (2026-06-18; including design to be settled
    before starting)**: differential comparison against raku of bind coherence across `my`/`our` ×
    `@`/`%` × element/whole/push/for-rw. Besides the 2 items above (#3252 `our` hash bind + the separate
    sink-warn #3253), **3 real bugs** remain (none is safe to fix in isolation; each needs design):
    1. ✅ **Fixed (#3255 + this session's follow-up) — whole reassignment of a bound hash + the symmetric
       constant element write**:
       `my %a := %b; %a = (z=>9)` died with "Cannot assign to a readonly variable (%a)" (raku also replaces %b with (z)).
       Root cause: **`readonly_vars` conflated 3 concepts**: ① the `:=` bind signal ② the immutability of `constant %M`
       ③ `is readonly` params.
       **Proper fix (adopted) = plant a dedicated marker `__mutsu_bound::%a`, separate from readonly, on `:=` bound
       containers** (new `Stmt::MarkBoundContainer`; the hash-bind desugar emits it; `compiler/stmt.rs` turns it into
       an env key at `SetGlobal`), and branch the readonly paths on that marker:
       - **Whole-reassign (#3255)**: the slot-path `CheckReadOnly` opcode (`vm.rs`) exempts marker-bearing vars.
       - **Whole-reassign inside a closure (this session)**: when a closure captures `%b` as a free var, the
         whole-reassign goes through the by-name `SetGlobal` path (`check_readonly_for_modify` in `vm.rs`) — added
         the marker exemption there too (`is_bound_container`). `lives-ok { %b = (z=>9) }` etc. no longer die.
       - **Symmetric bug (this session)**: the element path (`is_bound_hash_var`, `vm_var_assign_ops.rs`) treated
         **every** readonly `%` as bound (writable) = `constant %M<a>=9` wrongly succeeded. Narrowed
         `is_bound_hash_var` to require the marker; element writes to a marker-less readonly `%`-var (= `constant %M`)
         now die with X::Assignment::RO ("Cannot modify an immutable Int (1)" — matches raku). `constant @A` already
         died correctly (a separate array path).
       pin=`t/bound-hash-whole-reassign.t` (#3255, 10) + `t/constant-hash-element-ro.t` (this session, 14).
    2. **push non-propagation of param deref bind — real cause = non-sharing of `$x = @arr` (a deeper first-class
       container problem)**:
       `sub f($n){ my @a := @$n; @a.push(99) }; my @z=(1,2); f(@z); say @z.elems` gives mutsu 2 / raku 3.
       **Root cause isolated this session**: it is not a bind or writeback problem — **`my $n = @z` (assigning an
       array to a scalar) *copies* the Array in mutsu** (raku shares a reference to the same Array object =
       `@z.push` is visible through `$n`). `my $n := @z` (bind) and `my @a := @z` (array bind) share correctly in
       mutsu too = what is broken is **only `=` (assignment) when a scalar holds an array**. ∴ the `@$n` deref bind
       binds "the copied $n's array", so it never reaches the caller's @z.
       **Fix = have `$x = @arr` share the array's *outer cell* (Raku's reference-type semantics)** = a
       **high-blast-radius first-class-container change** touching the COW semantics of `$x = @arr` in general
       (a different layer from the local deref of writeback site-A). The next major Stage 1 target.
       **Design = `docs/scalar-array-sharing.md` (design-first; user's choice 2026-06-18).**
       An additional probe revealed: `$n = @z` already shares the same Arc (`$n[0]=8` propagates to @z), but
       **structural mutations like `.push` COW-detach via `Arc::make_mut`** = the same problem as element writes
       remains for the whole container. Canonical solution = promote source/target to a shared `ContainerRef` cell
       (escape-aware).
    3. ✅ **Fixed (Stage 1 sub-slice 1b, this session) — bound array's for-rw not writing back**:
       `my @a := @b; for @a { $_++ }; say @b` left both @a/@b unchanged in mutsu (raku increments both).
       **True cause identified**: `my @a := @b` **cell-ifies @a/@b onto the same `ContainerRef` cell** (which is
       why push/element writes propagate). But the for-rw writeback (the array branch of
       `write_back_for_topic_item`/`write_back_for_rw_param`) directly matched the return value of
       `get_env_with_main_alias(source)` against `Value::Array` **without a deref** = it missed the ContainerRef
       and skipped the writeback entirely (blast-radius site A). Fix = `deref_container()` the return value before
       reading the inner Array, and **write the rebuilt array through the shared cell** (the
       `write_back_container_source` helper = `cell.lock().clone_from` for ContainerRef, the previous
       set_env+update_local otherwise). Covers topic (`$_`) / named rw param (`-> $x is rw`) / multi-param rw
       (`-> $x, $y is rw`) / bound chains (`@a:=@b:=@c`) / named `@`-param `.push`.
       Writeback for non-bound arrays is unchanged. **This is the first slice where Stage 1 demonstrated the
       "outer-cell writeback chokepoint" = making site A ContainerRef-aware.** pin=`t/bound-array-for-rw.t` (15).
       ✅ **The hash version of site A is also fixed (this session's follow-up)**: bound **hash** for-values-rw
       (`for %h.values { $_++ }` / `-> $v is rw` / `%h.kv -> $k, $v is rw`) was broken by the same deref-less
       match. **3 sites** made deref-aware:
       ① hash-key pre-capture (loop setup, `hash_keys_for_writeback` = matching `get_env_with_main_alias(source)`
       directly against `Value::Hash` → `None` when bound → the true cause of the entire writeback being skipped)
       ② `write_back_hash_value_item` (topic path) ③ the hash branch of `write_back_for_rw_param` (kv/values).
       All now go via `deref_container()` + `write_back_container_source`. pin=
       `t/bound-hash-for-values-rw.t` (11). **∴ site A for-rw is complete for both bound arrays and hashes.
       Remaining = bug② (deref bind).**
    - These are candidates that Stage 1 (outer cell-ification) solves structurally, but 2/3 could also be handled
      by per-bug aliasing fixes. 1 is **done** as above, separating element/whole/closure paths with the
      `__mutsu_bound::` marker (a physical refactor splitting `readonly_vars` was unnecessary — the marker provides
      the logical separation). The remaining deep walls are only 2/3.
    - **[2026-06-18 pre-Stage-1 differential probe, narrowing]** exhaustive comparison of bound-container operations
      against raku confirmed that **only bug② (deref bind) and bug③ (for-rw) are user-visibly broken**:
      - `my @a := @b`'s **push / element assignment / reverse push already propagate correctly** (shared Arc +
        in-place mutation).
      - `my %h := %g`'s element writes also propagate correctly.
      - **Cases where a closure captures and mutates `@`/`%` (`my &f = { @a.push(9) }` / getter-setter factory)
        are already correct** — `box_captured_lexicals` skips `@`/`%`, but Array/Hash propagate via Arc sharing +
        the `env_dirty` reverse pull, so no bug arises. **∴ Stage 1 sub-slice 1c (extending closure capture to
        outer containers) has no user-visible bug = pure architectural cleanup (tends to be inert), low priority.**
      - Tracking bug③'s writeback suppression: even when bound, `for @a` emits `TagContainerRef("@a")` and
        `writes_back_topic` is true = the writeback is reached. But with a bound @a, topic mutations
        (`$_++` / `$_=99`) reach neither @a nor @b (raku updates both). `write_back_for_topic_item` reads
        `get_env_with_main_alias("@a")` without a deref and writes the rebuilt array via
        `set_env_with_main_alias` (no readonly check) — **and @a is still unchanged** = due to the interaction
        of snapshot iteration with bound readonly/shared storage (why session 19's localized fix didn't work).
        **∴ bug③ cannot be closed by a localized writeback patch = it presupposes Stage 1's outer cell
        (@a's elements truly rw-aliasing @b's storage).** This is the first implementation target of "starting" =
        **sub-slice 1b**.
- **Stage 1 — cell-ify outer containers escape-awarely**: promote to an outer `ContainerRef` only those slot
  containers that are "captured / `:=`-bound / `is rw` / `.VAR`-taken / mutated cross-frame", with env↔locals
  sharing the same cell. Bare locals keep the traditional Arc sharing (avoiding the perf cliff). Extend the
  escape analysis (#2758) verdicts to outer containers.

  **[2026-06-18 pre-start audit fixed the sub-slices]** The READ side is **already deconted** via `into_deref()`
  (GetLocal/GetGlobal/GetArrayVar/GetHashVar) / `resolve_array_entry`/`resolve_hash_entry` (elements) = the main
  risk of open-q#1 is concentrated in **write/mutation/writeback sites** (places that directly match raw
  `Value::Array(..)`/`Value::Hash(..)` from `env_mut().get_mut(name)` / `env_root_descended_mut(name)` /
  `get_env_with_main_alias(name)`). The ~14 sites the audit enumerated (by category):
  - **A. for-rw topic writeback**: `vm_control_ops.rs` `write_back_for_topic_item`(2078) / hash versions(2257,2327,2341)
    — matches `get_env_with_main_alias(source)` without a deref. The very writeback path of bug③.
  - **B. push / direct element mutation**: `vm_data_ops.rs` `@a.push` fast path(615) `env_mut().get_mut`, junction
    index assign (`vm_var_assign_ops.rs` 3000/3006).
  - **C. bound-index meta tracking**: `vm_var_assign_ops.rs:2966`, `vm_var_ops.rs` is/mark/remove_bound_index(598/607/619).
  - **D. simple array/hash op fast path**: `vm_var_assign_ops.rs` `exec_simple_array_op`(4200)/`exec_simple_hash_op`(4278)
    raw-match via `env_root_descended_mut`. `is_simple_op_target`(4196) raw-matches `self.locals[slot]`.
  - **E. initialization/coercion checks**: shaped-array push check (`vm_data_ops.rs:508`), hash type checks (2994/3130),
    push-assign array length fetch (3353).
  - **F. delete/exists**: `vm_var_delete_ops.rs` delete_hash(135)/array_exists(295/311).
  - **G. `%*ENV` reads**: `vm_misc_ops.rs:1673/1695` (internal, fixed name, so not a boxing target = can be left alone).
  - **H. thunk capture optimization**: `vm_arith_ops.rs:118` (read; adding a decont makes it a no-op).

  **Sub-slice 1a — write chokepoints first (behavior-preserving) = ✅ effectively complete (confirmed by the
  2026-06-19 audit)**: the initial plan was to add deconts to each of sites A–F in one sweep, but a **read-only
  clean audit found "the only remaining raw-match write site is junction index-assign"** — the rest were already
  ContainerRef-aware:
  - **A (for-rw writeback)** = already converted to `write_back_container_source`/`deref_container` (#3259/#3260).
  - **D (nested index-assign)** = `env_root_descended_mut` already descends cells via `descend_container_ref`.
  - **B's push (`vm_data_ops.rs` `@a.push`)** = the `is_simple_array` early guard routes bound cells to the
    `ContainerRef` arm (the cell-COW push near 577), so only plain Arrays reach the fast path (634) = **no gap**.
  - **F (delete)** = `exec_delete_index_named_op` is cell-aware via temporary unwrap→delete→write-back.
  - **C (bound-index meta) / E (init/coercion checks)** = internal meta hash or read-only checks where
    `ContainerRef` cannot occur = safe.
  - **The only remaining real gap = junction index-assign (B)**: `@a[0|1]=v` / `%h{'x'|'y'}=v` raw-matched without
    dereffing a bound cell (the hash arm was a real bug overwriting the cell with an empty hash and severing the
    bind). **Fixed in #3279 by unifying through `env_root_descended_mut`**
    (plain is byte-identical; bound matches raku). pin=`t/junction-bound-cell.t` (10).
  - **∴ Sub-slice 1a is complete.** The originally envisioned generic `env_container_mut(name) -> guard` helper
    turned out to be unnecessary (the existing `env_root_descended_mut` + `write_back_container_source` + push's
    cell arm cover the consumption surface). Next is Sub-slice 1b.

  **Sub-slice 1b — cell-ify `:=` bound arrays/hashes at the outer level (structurally resolving bugs ②③)**:
  extending the `__mutsu_bound_array_len::`/`MarkBoundContainer` markers that the bind desugar currently plants,
  make the bound array/hash's slot+env hold **the same `ContainerRef` cell**, so that `@a := @b` shares the outer
  structure too (extend the escape analysis `needs_cell` to `:=` bound containers, or box directly at the
  bind-establishment site). With this, the for-rw writeback (A) reaches @b through the cell, and the push
  non-propagation (②) is resolved as well. Sub-slice 1a's write chokepoints are the prerequisite.

  **Sub-slice 1c — extend escape-aware closure capture to outer containers**: relax `box_captured_lexicals`
  (`vm_register_ops.rs:333-377`), which currently explicitly skips `@`/`%` sigils and `Value::Array`/`Hash`, so
  that it boxes Array/Hash locals that are on `needs_cell_locals`. Bare locals stay as before (perf).
- **Stage 2 — removing the `env_dirty` dependence**: for containers whose divergence has structurally disappeared,
  make the reverse pull unnecessary. Measure first
  (confirm via `MUTSU_VM_STATS` that reverse-sync container staleness drops to 0).
  - **[2026-06-19 started, measured] `:=` bound containers turned out to be already coherent (no extra
    cell-ification needed) + fixed a measurement gap (PR #3296).**
    The whole-container bind of `my @a := @b` **already installs the same `ContainerRef` cell in both env and locals**
    (the `:=` body in `vm_var_assign_ops.rs` ~6415-6493: the same cell in the slot, env, and saved-frame of both
    source and target). So sub-slice 1b's main goal was **effectively already achieved by the existing bind code**.
    However, the reverse-sync survey's `cheaply_unchanged` (`vm_method_dispatch.rs`) had no `ContainerRef` arm and
    misjudged identical cells as "changed" via the raw `_ => false` — so bound containers were overcounted as
    effective stale, and `merge_method_env` was spuriously setting `env_dirty` when returning the identical cell.
    **Fix = `(ContainerRef(a),ContainerRef(b)) => Arc::ptr_eq(a,b)`**.
    The bound-container probe went `effective=6→0 / spurious=6`; hot benches (post-fix) =
    bench-fib/hash/string/method-call at `effective=0`, bench-class=`effective=1` (slot `desc` = one boundary pull
    of a method-return local), bench-array=`effective=1` (slot `@arr` = one boundary pull of a top-level push).
    **∴ container reverse-sync has vanished; the remaining genuine reverse-syncs are just 1–2 per-bench
    scalar/attr boundary pulls (not per-iteration).**
  - **Next target (remaining work toward Stage 3) = convert these boundary scalar/attr pulls (R1: method attribute
    writeback `desc`, by-name array write `@arr`) into precise write-throughs so `env_dirty` is never set.** These
    happen once at a boundary, not per hot-loop iteration, so the perf impact was tiny to begin with = the value of
    deleting `env_dirty` is **architectural (removing dual-store complexity), not perf** (confirmed by measurement).
    + Stage 1 sub-slice 1c (extending closure capture to `@`/`%` outers) has no user-visible bug (already works via
    Arc+env_dirty) = low priority, to be tidied together with the `env_dirty` deletion.
  - **[2026-06-19 session 27] ★Full census of the reverse-sync dependence surface (correctness view) = the true
    scale of Slice F revealed.** Stage 2's hot-bench measurement was a **perf-view** indicator (no per-iteration
    pulls), and after the `:=` bound container coherence it read as "1–2 remaining boundary pulls". But taking the
    **correctness view** — what does the reverse pull actually hold up — by experimentally no-op'ing the body of
    `sync_locals_from_env` (the `self.locals[i]=val` write-back) and running all 949 `t/` files, the inventory
    showed **about 80 files break (across categories)**. The hot benches (bench-array/bench-class) still **answer
    correctly** with the no-op (already coherent via forward write-through; effective=1 was the `cheaply_unchanged`
    distinct-Arc overcount) = **the benches do not represent the dependence surface**. The actual dependence
    categories (number of broken files):
    1. **carrier (EVAL/regex/`s///`/`let`/`temp`)** ≈8: `eval-carrier-precise-writeback` `regex-m-s`
       `regex-declarative-modifiers` `smartmatch-env-dirty` `single-store-slice-c-prime` `subst-closure-writeback`
       `let-temp` `keep-undo`. = R2 carrier writeback (the very area where the pairs/slip generalization is
       deferred at the deep-cell wall).
    2. **supply/react/whenever/gather/promise/proc-async (concurrent carriers)** ≈22: `supply-*`(10) `whenever-*`(2)
       `react-*`(1) `gather-*`(3) `promise-combinator` `proc-async`(4) `concurrency-*`(2) `io-watch` `scheduler-cue-times`.
       = coroutine/scheduler re-entry writes caller lexicals via env, observed after resumption via the reverse pull.
    3. **bound container / cell / rw-param** ≈15: `element-bind-cell`(11) `for-pairs-value-quanthash-writeback`(13)
       `rw-param-shared-cell`(5) `pair-value-*` `bound-container-pair-writethrough` `*-param-container-share`
       `map-native-rw` `is-rw-traits` `role-is-rw` `lvalue-subroutines-rw-proxy` `slurpy-*`.
    4. **scoped overlay / method / named-call dispatch** ≈14: `scoped-overlay-*`(5) `method-env-dirty`(4)
       `named-call-env-dirty`(6) `zeroarg-env-dirty`(6) `multitier-overlay-env` `say-env-roundtrip`
       `bareword-assign-expr` `statement-modifier*` `done-paren-stmt-modifier`(4).
    5. **attribute / native-backed instance** ≈7: `native-array-backed-instance`(**18/20**) `attribute-trait-mod`(5)
       `attribute-defaults-and-set-build` `class-var-namespace-separation` `methods-instance-regressions`(4)
       `definite-return` `return-method`.
    6. **closure** ≈3: `closure-container-capture` `closure-nested-writeback` `wrap-closure-capture`.
    7. **misc** ≈10 (one-offs: dynamic-var/nil/catch/metaop/import/require/return-exception etc.).
    **∴ Slice F is not "refining the boundary scalar/attr pulls" but a multi-session effort of folding the by-name
    writers of the 7 categories above, one by one, into precise write-through / cell sharing.** In particular
    categories 1/2 (carrier, concurrent carrier) share the root with the **deep-cell wall** that
    docs/vm-single-store.md has repeatedly hit (pairs/slip carrier-drop breaking `element-bind-cell.t`), and
    env↔locals container coherence (this document's §4-A outer cell sharing) is the prerequisite. **The global
    deletion of `env_dirty` is the final move; until then the reverse pull remains as a backstop.** The benches'
    "1–2 pulls" phrasing is perf-context only; this survey is the first statement of the correctness-side scale of
    the dependence.
    - **[2026-06-19 session 27, slice 1] lvalue-method writeback as call-site write-through (removing part of
      category 3 from the dependence surface).**
      Wired `MUTSU_NO_REVERSE_SYNC=1` through `vm_stats::reverse_sync_disabled()` (cached; zero cost in release) and
      added a **permanent campaign diagnostic** that can skip the body of `sync_locals_from_env` (each slice's means
      of verifying "this test no longer depends on the reverse pull"). First write-through conversion =
      `__mutsu_assign_method_lvalue` / `__mutsu_index_assign_method_lvalue` (`$p.value = X`, `.value--`,
      `@a.head=`/`.tail=`/`.first(...)=`, `%h.AT-KEY(k)=`, accessor-based nested index). These runtime builtins
      mutated the target var by name via `self.env.insert(var, ...)` and left the local slot to the reverse pull.
      On the VM call side (`exec_call_func`), **capture the target var name (args[4]) before dispatch → after
      dispatch, write `env[var]` through to the local slot** (`HashSlotRef` slots are skipped, same as the reverse
      pull). Result (`MUTSU_NO_REVERSE_SYNC=1`): `t/role-is-rw.t` and `t/pair-value-container.t` **turned fully
      passing with OFF and are removed from the dependence surface**; `t/for-pairs-value-quanthash-writeback.t`
      went 13→12 (`$p.value--` resolved; the remaining 12 are the for-loop topic path = a separate slice).
      ON keeps all t/ green (the write-through equals the value the reverse pull would fetch = additive, no
      regressions). pin=`t/lvalue-method-writeback-coherence.t` (12).
    - **[2026-06-19 session 27, slice 2] for-loop topic writeback (`.value = X for $b.pairs`) as source-local
      write-through.**
      The body of `for $b.pairs { .value = X }` (mutable BagHash/MixHash/SetHash) emits
      `__mutsu_assign_method_lvalue($_,"value",..)`, and inside the builtin calls
      `quanthash_set_weight(&EMPTY_code, ...)` via `topic_source_var` (=`$b`). **Passing an empty `CompiledCode` is
      because the builtin path has no bytecode** (stated in a comment in methods_mut.rs) → only env[$b] is updated
      and the local slot is left to the reverse pull. Fix = in the for-loop (`exec_for_loop_body`, which holds the
      real `code`), after the loop ends, when the source is a mutable QuantHash, write the final env value of
      `container_binding` (=`$b`) through to the local slot (once at loop end, not per arm; `HashSlotRef` skipped).
      Result (`MUTSU_NO_REVERSE_SYNC=1`): `t/for-pairs-value-quanthash-writeback.t` went 12→**0** (13→0 combined
      with slice 1 = **the whole file removed from the dependence surface**). ON all t/ green (additive).
      **The core of the remaining category 3 is the deep `:=` cell (element-bind-cell 9/28/41-47) = presupposes
      §4-A outer cell sharing.**
    - **[2026-06-19 session 28, slice 3] `is rw` / `is raw` parameter writeback as call-site write-through.**
      `sub f($a is rw){ $a = 42 }; f($x)` was a **real bug** where `$x` never got updated (failing even with the
      reverse pull ON). Root cause = the rw parameter `$a` is bound in the body to a slot-only local
      (`GetLocal`/`SetLocal`), and `$a = 42` **never reaches env** (`flush_local_to_env` does not mirror
      slot-only locals with `needs_env_sync=false`) → at return time `apply_rw_bindings_to_env` read a **stale
      env[a]** and wrote it to the caller's source var. **(1) Correctness fix**: in the return path of
      `call_compiled_function_named`, before `pop_call_frame` (while `self.locals` is still the callee's), flush
      the rw parameters' final slot values to env → `apply_rw_bindings_to_env` writes the correct values.
      **(2) Slice F (making OFF work)**: record the caller-source names that dispatch wrote back in the new field
      `pending_rw_writeback_sources`, and the call-site op (which holds the caller's `code`) writes each source's
      env value **through to the caller local slot** via `apply_pending_rw_writeback(code)` (`HashSlotRef` skipped).
      Covers both the sub function dispatch (`call_compiled_function_named`) and pointy-block / closure dispatch
      (`call_compiled_closure_with_topic`); draining is wired into each call site:
      `exec_call_func`/`CallOnValue`/`CallOnCodeVar`/carrier (`ExecCall*`). The field is cleared at dispatch entry,
      preventing leaks from sibling paths that never drain.
      Result (`MUTSU_NO_REVERSE_SYNC=1`): `t/is-rw-traits.t` (sub rw, pointy-block rw) fully passes with OFF.
      pin=`t/rw-param-writeback-coherence.t` (16: scalar/compound/nested-forward/is-raw/pointy-block/loop/2-rw/mixed;
      ON=OFF=matches raku).
      **Remaining category 3: lvalue sub/rw Proxy store (`sub () is rw`, `Proxy.new`), method rw params
      (`vm_method_dispatch.rs` rw_writeback), pair-value element write-through (hash values, `$p.value<f>=`),
      map captured-var.**
    - **[2026-06-19 session 29, slice 4] `is rw` / `is raw` *method* parameter writeback as call-site write-through
      (reusing slice 3's mechanism for method dispatch).**
      `class C { method m($a is rw){ $a = 9 } }; C.m($x)` worked with ON but failed with OFF (reverse-pull
      dependent). `call_compiled_method` (`vm_method_dispatch.rs`) writes the rw_writeback into the result of
      `merge_method_env` at return time, updating env under the caller-source name, but the caller local slot was
      left to the reverse pull. Reused slice 3's `pending_rw_writeback_sources` mechanism: `call_compiled_method`
      records the rw_writeback source names in the field (cleared at entry; leak prevention) → the
      `CallMethod`/`CallMethodMut`/`CallMethodDynamic(Mut)` ops (which hold the caller `code`) write through via
      `apply_pending_rw_writeback(code)`. **Also fixed a pre-existing bug**: `has_rw_params` only tested `"rw"` and
      missed `"raw"`, so a method with only `is raw` rode the fast path / merge skip and its writeback was dropped
      (failing even with ON) → extended to `"rw" || "raw"` (symmetric with sub dispatch; raw is also written back
      via rw_bindings when the arg is an lvalue).
      Result (`MUTSU_NO_REVERSE_SYNC=1`): scalar/compound/swap/instance/raw method rw params fully pass with OFF.
      pin=`t/method-rw-param-writeback-coherence.t` (13).
      **Remaining category 3: lvalue sub/rw Proxy store (`sub () is rw`, `Proxy.new`), pair-value element hash form
      (`$p.value<f>=` = §4-A container-identity deep wall; simple write-through impossible), map captured-var.**
    - **[2026-06-19 session 30, slice 5 (closure category)] Closure captured-variable write-through.**
      The captured scalar `$sum` of `@a.map({ $sum += $_ })` was ON=6 / OFF=0 (reverse-pull dependent).
      `call_compiled_closure_with_topic` (`vm_closure_dispatch.rs`) writes changed free vars back to
      `restored_env` (=`self.env`) at body end, but the caller local slot was left to the reverse pull. Reused the
      `pending_rw_writeback_sources` mechanism of slices 3/4: right after the closure dispatch's caller-writeback
      scan, compare against the entry-time snapshot (`free_at_entry`) and record in the field **the changed free
      vars that are caller lexicals (`restored_env.contains_key_sym`)** (excluding `$_`/`@_`/params) → the
      `CallMethod`/`CallOnValue`/`CallOnCodeVar`/`ExecCall` ops (holding the caller `code`; wired in slices 3/4)
      write through via `apply_pending_rw_writeback(code)`. The field clear added at closure-dispatch entry in
      slice 3 prevents leaks.
      Result (`MUTSU_NO_REVERSE_SYNC=1`): direct `$blk()`, array `.map`, pointy blocks, per-iteration closures, and
      captured `@`-push all pass with OFF.
      pin=`t/closure-captured-var-writeback-coherence.t` (10); `t/map-native-rw.t` test 15 also resolved for OFF.
      **This slice covers only the VM closure dispatch. Range `.map` / `.grep` go via interpreter dispatch
      (`call_sub_value`) — a separate follow-up (captured hash element `%h{$k}=` via block fails even with ON =
      pre-existing separate bug).**
    - **[2026-06-19 session 30 second half, range map/grep DEFERRED] captured-var for range `.map` / `.grep` is
      blocked by lazy eval.**
      `(1..5).map({ $s += $_ })` returns a **lazy pipe** via `dispatch_map_method` →
      `is_lazy_pipe_source`→`make_lazy_pipe`, and the block runs **at Seq forcing time (sink context)**, not at the
      `.map` CallMethod op, so the CallMethod drain cannot capture the block's captured-var changes. Adding
      recording to `call_sub_value` (both merge_all true/false branches) had no effect (block execution is deferred
      to sink; different scope). **OFF coherence for lazy pipes needs a different design that drains at the
      sink-forcing site = deferred.** The attempt was reverted.
    - **[2026-06-19 session 31, slice 6] lvalue-return write-through for `is rw` subs.**
      `my $f = sub () is rw { $v }; $f() = 9` updates `$v`, but only with ON (with OFF the `$v` slot is stale).
      `$f() = 9` compiles to the `__mutsu_assign_callable_lvalue` builtin, and `assign_rw_target_expr`
      (`builtins_lvalue.rs`) wrote the target var (`$v` / forwarded `$leaf()`→…→`$v` / `return-rw` target) by name
      via `self.env.insert(name, value)`, leaving the caller slot to the reverse pull.
      Record the var names written in `assign_rw_target_expr`'s `Expr::Var` and `return-rw` branches into
      `pending_rw_writeback_sources` → the ExecCall drain of `__mutsu_assign_callable_lvalue` (wired in slice 3)
      writes through to the caller slot. Forwarding (nested rw subs) reaches the final Var by recursion and records it.
      Result (`MUTSU_NO_REVERSE_SYNC=1`): `t/lvalue-subroutines-rw-proxy.t` (5) fully passes with OFF.
      pin=`t/rw-sub-lvalue-writeback-coherence.t` (8).
    - **[2026-06-19 session 31, slice 7 (attribute-native-instance category)] receiver write-through for mutating
      methods.**
      For `class Stack is Array {}; my $s = Stack.new; $s.push(1)`, the receiver `$s` was coherent only with ON
      (with OFF `$s.elems` is stale). The native mutator group of `exec_call_method_mut_op`
      (`vm_call_method_mut_ops.rs`) (`$s.push`/`.pop`/`.unshift`/… on Array-/Hash-backed instances plus ~15
      `env_mut().insert(target_name, ..)` branches) reassigned the receiver into env by name and left the caller
      slot to the reverse pull.
      In the `CallMethodMut` op (`vm.rs`; holds the caller `code` + `target_name_idx`), **push the receiver name
      onto `pending_rw_writeback_sources` after dispatch** → the existing `apply_pending_rw_writeback(code)` drain
      (wired in slices 3/4) writes through to the caller slot (the HashSlotRef-skip invariant same as the reverse
      pull). Pushing the receiver name on every CallMethodMut = reproducing the reverse pull's per-slot behavior
      restricted to 1 receiver (maximal blast radius, but no regressions across make test 9314 + a 181-file roast
      sample).
      Result (`MUTSU_NO_REVERSE_SYNC=1`): `t/native-array-backed-instance.t` (20) goes OFF 2→20, fully passing.
      pin=`t/mut-method-receiver-writeback-coherence.t` (12: Array/Hash-backed instance, plain-array regression guard).
    - **[slice: 0-arg fast-call captured-outer write-through (PR #3317)]**: calling
      `sub bump { $acc = $acc + 5 }` as `bump()` (0-arg) left the captured-outer `$acc` writeback stale with OFF
      (#3307 covered only closure dispatch; 0-arg named subs take the separate `call_compiled_function_fast` path).
      At `call_compiled_function_fast` exit, push `cf.code.free_var_writes` (excluding topic `_`/`@_`/`%_`) onto
      `pending_rw_writeback_sources` → drain at the fast-call site via `apply_pending_rw_writeback`.
      **Single-level (direct caller) only.** **Multi-frame (nested 0-arg / 0-arg recursion) needs retention
      (propagating un-placeable sources to ancestors), but retention perturbs lazy-iteration topics and makes
      `grep(*.is-prime, 1..Inf)` loop forever = the same lazy-eval wall as session 30's range map/grep = deferred.**
      Removed `t/single-store-slice-a.t` from the OFF dependence surface.
      pin=`t/fast-call-captured-write-coherence.t` (10).
- **★Dependence-surface re-measurement (session 32; important correction)**: "`t/` + whitelisted roast are 100%
  reverse-sync independent" (the session-31 milestone) was **wrong due to a measurement error**. Trying Stage 3a
  (disabling `sync_locals_from_env` by default) **regressed 65 `t/` files** (all ON pass / OFF fail = genuinely
  dependent). The previous diff scan was a false negative — the `MUTSU_NO_REVERSE_SYNC` toggle had not actually
  been taking effect.
  Breakdown: concurrency 21, **ordinary language features 21** (catch-in-blocks/definite-return/statement-modifiers/
  return-exception/slurpy/attr-trait/import/require/role-body etc.), env-dirty/overlay pins 14, carrier 6,
  closure 5, deep-cell 2. **∴ Full Stage 3 removal is not "within a 2-4 session range"; a large grind remains of
  folding the by-name writers of 65 files into write-through/cell sharing** (the "7 categories, ~80 files" framing
  of survey #3299 is the correct one). Lesson: before believing "0 dependents", sanity-check that the toggle
  actually changes behavior on a known-dependent case (`t/zeroarg-env-dirty.t`).
- **Stage 3 — Slice F proper + pairs/slip carrier-drop generalization**: delete
  `env_dirty`/`ensure_locals_synced`/`sync_locals_from_env`/`saved_env_dirty` (`docs/vm-single-store.md` Slice F).
  Simultaneously enable the `pairs`/`slip` carrier-drop (safe with outer cell sharing). **Prerequisite = the
  65-file dependence above dries up via write-through/cell sharing.**

Each Stage is pinned down with **`t/element-bind-cell.t` (54) / `t/array-bind-cell.t` / `t/hash-bind-cell.t` /
nested.t 43, plus a main-vs-branch comparison of release roast, plus wall-clock of int.t/method-call**
(the #2746 lesson: perf regressions are undetectable by make test and only surface as CI release roast timeouts).

### [2026-06-20 session 35] Precise root cause of the deep-cell wall (target of the next sub-slice fixed)

Bisected and root-caused the OFF failures of `t/element-bind-cell.t` (tests 9/28/30/31/41/42-47). **The entrance to
the wall is pinned down exactly to the case "a scalar holds a container, and a 2+-level element of it is LHS-bound
with `:=`"**:

- `@a[1] := $v` (**array VAR** receiver, any depth) = works with OFF (array vars already have the env↔slot shared spine).
- `$s[1] := $v` (**scalar-holding-array**, 1 level) = works with OFF.
- `$s[1][1] := $v` (scalar-holding-array, **2 levels**) = with OFF, **`$v=NEW` does not propagate to the element**
  (the element is the stale old value).
- `$s[1][1][0] := $v` (**3 levels**) = in addition, **the sibling `$s[1][1][1]` is destroyed to `Nil`** (the whole
  inner array diverges).

**Mechanism**: deep computed-target bind (`vm_var_assign_ops.rs:4933` `Phase 2 Stage 2`) mutates the resolved
`target` (the inner container) in place via `arc_contents_mut` to embed the cell, and writes the cell back to the
source var `$v`. But `target` is an **env-side copy** obtained by traversing `$s`, and `make_mut`/COW during the
descent **diverges it from `$s`'s slot-side Arc**. The cell gets embedded only in env; the slot is stale. With ON,
the next read of `$s` re-converges the slot from env via the reverse pull; with OFF it never re-converges.
`@a[1]:=` works because array vars traverse the shared spine of `env_root_descended_mut`.

**∴ The next sub-slice (substrate) = "when the root of a deep `:=` element bind is a scalar-holding-container,
promote that scalar to a shared `ContainerRef` cell (the `:=`-target version of #3264's scalar-array-sharing)"**.
Then the bind descent (`env_root_descended_mut`) traverses the env↔slot shared spine, and the cell embedding is
visible from both stores. **High blast radius; needs the `:=`-target extension of the escape analysis + cell
promotion in the descent** = should be tackled in a focused independent session (do not rush it at the end of one).
pin candidate = making `t/element-bind-cell.t` OFF-clean (tests 9/28/30/31/41/42-47). **RHS bind
(`$b := $s[1][1][0]`) works with OFF** = only LHS (element := scalar) diverges.

---

## 6. Open questions to settle before starting

1. **Coverage of the decont consumption surface for outer container cells**: a complete inventory of paths where a
   ContainerRef-outer could leak in value ops. Broader than Phase 2's leaf cells (outers are directly walked by
   iteration/slice/native methods over items).
2. **Extending the escape verdict to outers**: widen the existing `captured_mutated_locals`/`needs_cell_locals`
   (scalar-centric) to Array/Hash locals. Verify with timing the perf of the common case of a closure capturing and
   mutating `@a`.
3. **Interaction with the scoped overlay chain**: how scoped_child/flatten treat an outer cell that lives in a
   parent tier (the cell should survive via Arc bump, but the flatten's merged map needs checking).
4. **Cross-thread**: consistency of `clone_for_thread` / `shared_vars` with outer cells (`Arc<Mutex>`)
   (overlaps with Track C).
5. **Non-container dependence of `env_dirty`**: does the "implicit reconcile dependence of `:=` bind etc."
   (scalar side) found in Slice B remain after outer cell-ification? If so, Slice F also needs the scalar-side
   refinement (an extension of Slice C/C′).

---

## 7. References

- `docs/vm-single-store.md` — the forward plan of dual-store unification (Slices A–G). This document is the design
  of Slice F's prerequisite.
- `docs/vm-dual-store.md` — the mechanism map and the archive of retracted attempts.
- `docs/container-identity.md` — the first-class container implementation ledger (Phases 0/1/2/3). Outer
  cell-ification is an extension of Phase 2.
- `docs/slotref-removal-plan.md` — SlotRef removal (adjacent to leaf cell-ification).
- Demonstration of the failure: `t/element-bind-cell.t` (9/28/41/44/46/47 break under pairs/slip carrier-drop).

---

## 6. Multi-frame captured-outer propagation — the next substrate handoff (2026-06-20; session-36 reconnaissance complete)

The largest remaining wall after session 36 swept the single-frame + deep-cell OFF dependences (36→13).
**Pinning down the canonical case, experimental results, and next investigation points for immediate pickup.**

### 6.1 Canonical case (representative of the remaining OFF dependences)

```raku
my $acc = 0;
sub bump-outer() { $acc = $acc + 10 }   # writer (innermost frame; $acc is a free var)
sub via()        { bump-outer() }        # mid frame (does not hold $acc)
via();                                    # ← alone, raku=10; even OFF gives 10 (already correct!)
via();
say $acc;                                 # raku=20; OFF=10 (accumulation failure)
```

Affected OFF-dependent files (6 of the 13): `closure-nested-writeback`(2) `multitier-overlay-env`(1)
`scoped-overlay-env-dirty`(1) `wrap-closure-capture`(3) `zeroarg-env-dirty`(1) `slurpy-is-raw`(1).
All of the "nested ... propagate captured mutation" family.

### 6.2 ★The most important isolation (session-36 probe)

- **A single `via()` is correct even with OFF** (`acc=10`). ∴ **one round of caller→mid→writer propagation works
  with the existing mechanisms** (#3317's 0-arg fast-call captured-write drain, etc.).
- **What breaks is the cross-call ACCUMULATION of `via(); via()`** (stays 10; never becomes 20).
  = the **same kind** as session 34's "method-invocant autothread `our`-var cross-call accumulation" wall
  (memory §34). The problem is not "no propagation" but "the second call cannot accumulate on top of the
  first call's write".

### 6.3 ★Conclusion of the retention experiment (session 36; reverted)

Tried **guarded retention** in `apply_pending_rw_writeback` (`vm_env_helpers.rs`):
re-push source names absent from the current frame onto `retained` (only when `call_frames` is non-empty +
topic/match names excluded + `env.contains_key`) → the parent frame's drain picks them up. Results:

1. **It does not fix the canonical case** (still `acc=10`). ∴ retention (reinforcing reverse propagation) is
   **off target**. The wall is on the forward side (accumulation).
2. **It does not reproduce #3317's lazy hang** (`grep-lazy-range.t` exits 0).
   ∴ with the topic/match exclusion + `call_frames` guard, **the lazy-eval collision is avoidable** (#3317's wall
   is de-risked). — If retention ever becomes necessary in the future, this guard is the right safety-valve design.

### 6.4 Next investigation points (forward-accumulation hypothesis)

Strong suspicion that when the second `via()`→`bump-outer` reads `$acc`, it **reads a stale value (0 instead of 10,
or locals instead of env)**. Candidates:
- **The forward sync at call entry (locals→env) overwrites env `$acc` with a stale locals value** before the writer
  reads it → after the 1st call, locals `$acc` is not updated (the reverse drain is a no-op because the `via` frame
  has no `$acc` local; the top-level drain is called but…) → at the 2nd call's entry, the stale locals are flushed
  to env → the writer re-writes 0+10=10.
- ∴ next session: pin down **the `$acc` value bump-outer reads** and **env↔locals `$acc` at via entry /
  bump-outer entry** with tracing (`MUTSU_TRACE` or one pass of temporary eprintln) → then plug the forward sync's
  stale overwrite.
- Related existing analysis: session 34's `our`-var cross-call (method-autothread; a partial fix was not viable and
  was reverted). If it is the same accumulation mechanism, a common solution is possible.

### 6.5 Breakdown of the remaining 13 (end of session 36; for reference)

- **multi-frame accumulation (this section, 6 files)** ← the next substrate.
- **regex carrier (2)**: `regex-m-s`'s `s///`-topic `$_` writeback, `regex-declarative-modifiers`'s `:let`.
- **concurrent cell (5)**: `done-paren-stmt-modifier`/`gather-infinite-coroutine`/`react-whenever-last-next`/
  `supply-on-demand-closing`/`supply-sync-infinite-emit` (another thread → caller slot unreachable; presupposes
  §4-A cell sharing).

### 6.6 ★Session 37 — multi-frame accumulation solved (OFF 13→7)

§6.4's "forward accumulation hypothesis" was **half right**. The real cause, found via actual tracing
(entry/exit env of `call_compiled_function_fast`, GETLOCAL/SETLOCAL of the `acc` slot):

- In the canonical `via(); via()`, **top-level `$acc` has not only env but also a local slot** (`code.locals=["acc"]`).
  `say $acc` **reads the slot via GetLocal**. env ends up correctly at 20, but the slot is stale (10).
- **Real cause = asymmetry between the slow path and the cached fast path**. The slow `exec_call_func_op` calls
  `if env_dirty { reconcile_locals_from_env_at_site(code) }` after `dispatch_func_call_inner`, re-converging the
  caller slots from env (this reconcile is **not covered** by the `reverse_sync_disabled` toggle = it runs even
  with OFF). But **the cached fast paths (positional-light / light / 0-arg fast / OTF), closure calls
  (`exec_call_on_value_op`/`exec_call_on_code_var_op`), the wrap chain, and lexical-override return early and
  skipped this reconcile**. So the 1st call (cache cold = slow) gets reconciled, but the 2nd (cache warm = fast)
  skips it, freezing the slot at 10. `apply_pending_rw_writeback` is precise but single-frame (a nested callee's
  write is drained at the intermediate `via` frame and vanishes one level too deep), so it does not reach
  multi-frame.
- **Why retention (§6.3) did not fix the canonical case**: the wall was not reverse propagation but the
  forward-side **asymmetry "cached fast-call sites lack the env_dirty reconcile"**.

**Fix (PR #XXXX)**:
1. Helper `drain_and_reconcile_after_cached_call(code)` (`vm_env_helpers.rs`) =
   `apply_pending_rw_writeback` + `if env_dirty { reconcile_locals_from_env_at_site }`.
   Byte-identical to the slow path. The env_dirty gate = pure calls (fib) pay nothing
   (positional-light merge sets env_dirty only on captured-outer writes).
2. Converted the 4 cached fast-path sites (positional-light / light / 0-arg fast / OTF) to the helper.
3. Closure calls (`exec_call_on_value_op`/`exec_call_on_code_var_op`), the wrap chain, and lexical-override set a
   blanket `env_dirty=true` at the end (the callee's writes cannot be tracked precisely), so they get an
   **unconditional reconcile** (that blanket dirtiness is exactly what the reverse pull acts on with ON = consistent).
4. **Carrier completeness (the rw-param version of open-q#2)**: the rw writeback of
   `lives-ok { f(@a, $x is raw) }` writes env by name via `apply_rw_bindings_to_env` but is unlogged in the carrier
   set → `writeback_carrier_writes` misses it → `fully=true` drops env_dirty → slot stale. Fix = when recording
   rw-sources in `call_compiled_function_named`, also call `note_caller_env_write(source)` if a carrier is active.

pin=`t/multi-frame-accumulation-coherence.t` (10: named/closure/positional/array/string/carrier-slurpy;
ON=OFF=raku). make test PASS (981 files/9587 tests). OFF scan: **13→7** (remaining = regex carrier 2 +
concurrent cell 5 = the known walls).

**Breakdown of the remaining 7 (end of session 37)**:
- **regex carrier (2)**: `regex-m-s` (`s///`-topic `$_`), `regex-declarative-modifiers` (`:let`).
- **concurrent cell (5)**: `done-paren-stmt-modifier`/`gather-infinite-coroutine`/`react-whenever-last-next`/
  `supply-on-demand-closing`/`supply-sync-infinite-emit` (another thread → caller slot unreachable; presupposes
  §4-A cell sharing).

### 6.7 ★Session 38 — regex carrier solved (OFF 7→5; PR #3347)

Folded the 2 remaining regex-carrier items from §6.6. In both, the callee (the substitution engine / the
declarative regex prefix) wrote a caller lexical into `env` **by name**, bypassing the VM's slot write-through.

- **Bare `s///`'s `$_` topic (`regex-m-s`)**: `write_subst_topic_checked` (`vm_string_regex_ops.rs`) only inserted
  the modified topic into `env["_"]`, so when `my $_` has made `_` a compiled local slot, the slot goes stale.
  Fix = pass `code` into the function and, after the env insert, write through to the slot via
  `update_local_if_exists(code, "_", &result)` plus `note_caller_env_write("_")` to raise carrier/env_dirty.
  When `$_` aliases the source scalar of a `given`/`for` (`topic_source_var`), also mirror to that source name
  (symmetric with the `$x ~~ s///` smartmatch path).
- **The `:let $x = ...` declarative modifier (`regex-declarative-modifiers`)**: the declarative-regex prefix
  handler (`runtime/regex/regex_match_public.rs`) wrote `:let` values directly into `self.env`. On a successful
  match it does not apply `restore_on_fail`, so the values persist, but the caller slots were never updated.
  Fix = on match success, log the keys of `restore_on_fail` (= the `:let` names) together with their current env
  values into `pending_local_updates` (+ carrier) → the smartmatch site's `writeback_match_locals` re-converges the
  caller slots without a reverse pull.
  - **★Note**: `t/regex-declarative-modifiers.t` test 4 (expecting `$a==1` after variable restore following an
    unsuccessful `:let` match) — **raku itself returns `$a==5`** = the restore-on-overall-fail semantics of `:let`
    diverge between mutsu and Rakudo (a separate matter, unrelated to the dual store). The pin test drops this
    contested value check and asserts only the match result (nok).

pin=`t/regex-carrier-writeback-coherence.t` (10: bare `s///`/`s:g///`/`$x ~~ s///`/`:let` success/`:let` failure;
ON=OFF=raku). make test PASS (984 files/9614 tests). OFF scan: **7→5**.

**Breakdown of the remaining 5 (end of session 38) — all concurrent cell (presupposing §4-A cell sharing)**:
`done-paren-stmt-modifier`/`gather-infinite-coroutine`/`react-whenever-last-next`/
`supply-on-demand-closing`/`supply-sync-infinite-emit`. The callee runs on another thread, and the caller's
local slot lives in another frame / another stack, so a write-through physically cannot reach it. The only
solution is the design that promotes captured outers to a **shared cell (`Arc<Mutex<Value>>`-like)** so both
threads reference the same cell (§4-A). This is a substrate change distinct from the single-frame write-through
family.

Next substrate = concurrent cell sharing (§4-A). The single-frame / multi-frame / carrier write-through walls are
all gone with this; what remains is only cell sharing across thread boundaries.

### 6.8 ★Session 38 second half — the "concurrent cells" were actually synchronous (OFF 5→0; PR #XXXX)

The 5 remaining items that §6.6/§6.7 had classified as "concurrent cell (another thread → caller slot unreachable;
presupposes §4-A)" turned out, **by actual measurement, to all be synchronous execution on the same VM** (the
undeniable evidence being that reverse-sync ON fixes them via the pull = if they were another thread / another env,
ON would not fix them either). The whenever callbacks of `Supply.from-list` / `supply { emit }` and `gather`
coroutine bodies all run as compiled bytecode on `&mut self` and write captured-outer caller lexicals into `env`
by name. So they could be folded with the same mechanism as the single-frame write-through family.

- **react/whenever (4 items: `done-paren-stmt-modifier` / `react-whenever-last-next` /
  `supply-on-demand-closing` / `supply-sync-infinite-emit`)**: the post-event-loop reconcile of
  `exec_react_scope_op` (`vm_register_ops.rs`) was `sync_locals_from_env` (toggle-covered = a no-op with OFF).
  Replaced it with the **toggle-independent `reconcile_locals_from_env_at_site(code)`** (byte-identical with ON;
  same HashSlotRef/`!attr` skips). One line resolved 4 items.
- **gather coroutine (1 item: `gather-infinite-coroutine`'s `.first` captured-`$c`)**: in the incremental-pull loop
  of `try_lazy_gather_first` (`vm_native_first.rs`), the matcher closure (`* >= 3`) **overwrites
  `self.current_code` with its own frame** via `exec` and never restores it. `force_lazy_list_vm_n` captures this
  `current_code` as "the caller frame in which to reconcile captured-outer writes"
  (`reconcile_caller_after_lazy_force`), so the 2nd and later forces reconcile the matcher's frame and the caller's
  `$c` slot goes stale with OFF (stuck at c=1). With ON, the post-method barrier pull was masking it. Fix = pin the
  caller `current_code` at loop entry and **restore it right before each force**. The captured-`$c` of the
  `.head(n)` family is 0 even with ON (a separate constraint on gather-head side-effect visibility; unrelated to
  the dual store; the test does not assert it either).

pin=`t/concurrent-cell-writeback-coherence.t` (8: react `++`/`+=`/`done()` boundary, supply emit, gather `.first`
captured accumulation; ON=OFF=raku). OFF scan (all t/): the remaining failures are only the 2 regex items fixed
by #3347 = **the two PRs together take the OFF surface 7→0**.

**★Conclusion: the locals↔env coherence write-through grind is complete.** Sessions 27–38 folded all categories —
single-frame / multi-frame / carrier / concurrent (= actually synchronous) — into the 3 mechanisms
`pending_rw_writeback_sources` / `reconcile_locals_from_env_at_site` / carrier logging, and with OFF
(`MUTSU_NO_REVERSE_SYNC=1`) all t/ pass. `sync_locals_from_env` (the reverse pull) **no longer has any place where
it is needed for correctness** = Stage 3 (disabling reverse-sync by default → deleting the mechanism) is within
range for the first time. The wall of 65 files that regressed in session 32 has been fully write-through-ified by
this grind. The next big item = the Stage 3 retry (verify the default disabling of `sync_locals_from_env` across
all t/ + roast CI).

## 7. Stage 3 — disabling reverse-sync by default (end of session 38; in progress)

Invert the verdict of `reverse_sync_disabled()` (`vm_stats.rs`) = **the reverse pull is OFF by default**. The old
escape hatch `MUTSU_NO_REVERSE_SYNC` is removed, replaced by the opt-IN `MUTSU_REVERSE_SYNC=1` (re-enabling the old
behavior = for A/B diagnostics).

- **make test (all t/) = PASS** (985 files / 9622 tests, as the clean OFF scan predicted).
- **★OFF dependences unrepresented by the t/ scan remain on the roast side** (found via a local sample). roast
  pokes at rw-aliasing / coroutine-rw / lvalue-method-rw that t/ does not exercise:
  - `roast/S04-statements/gather.t` test 36: `my $l = gather { take-rw my $ = 1 }; lives-ok { $l.AT-POS(0) = 42 }`
    → **an rw assignment to a take-rw gather element inside a closure (`lives-ok {}`) does not propagate outward
    with OFF**. It propagates with a bare block `{ }` / a bare statement (the frame crossing is the wall). The
    anonymous container the coroutine took rw is not a shared cell but goes via the coroutine env = reverse-pull
    presupposing.
  - `roast/S04-blocks-and-statements/pointy-rw.t` test 8: `$pair.values should be rw (2)` = the frame crossing of
    an lvalue-method rw alias.
- **Policy (user's choice: full Stage 3, fix-forward with CI as the net)**: draft PR #3349. Obtain the complete
  list of roast-only OFF dependences via a local `make roast` (**14 items** after excluding flaky ones) → fix each
  forward with a toggle-independent reconcile.

### 7.1 Fix-forward of the 14 roast OFF dependences (13/14 solved)

Complete list (OFF=fail ON=pass confirmed with `MUTSU_REVERSE_SYNC=1`; the flaky set/baghash/junctions/
IO-Socket-Async excluded):

| # | file | root cause | fix |
|---|------|------|------|
| 1-9 | pointy-rw, gather(take-rw), S14-roles/{anonymous,parameterized-mixin,rw}, S12-meta/primitives, S04-terminator, S02-symbolic-deref, S32-hash/kv | **Block-taking Test functions** (`lives-ok{}`/`subtest`/`throws-like`) go through `exec_exec_call_pairs_op` because of the `__mutsu_test_callsite_line` named arg → the block runs in the interpreter → `is rw` for-aliases etc. are written to env by name (unlogged in the carrier). The pairs op's carrier fallback only set env_dirty=true without reconciling | `exec_exec_call_pairs_op` (unconditional) + the `!fully` branch of `exec_exec_call_op` get `reconcile_locals_from_env_at_site` |
| 10-11 | S14-roles/{mixin-6e,submethods-6e} | The `submethod BUILD/TWEAK` of `$obj does/but Role` writes captured outers by name. `exec_does_op`/`exec_does_var_op` used the **toggle-gated** `sync_locals_from_env`; `exec_but_mixin_op` had **no sync at all** | Replaced the two does ops with the toggle-independent reconcile; added a reconcile to but (passing `code` to the `ButMixin` op) |
| 12 | S32-str/substr-rw | The **Proxy STORE** of `$r := substr-rw($s); $r = v` writes the referent `$s` by name | Added a reconcile to the 3 Proxy STORE sites of scalar-assign (after `assign_proxy_lvalue`) |
| 13 | S03-operators/notandthen | `andthen`/`notandthen`'s `OpCode::CallDefined` dispatches a **user `method defined`** → captured `$calls++` | Reconcile only in CallDefined's user-method branch (the native check stays pure) |
| **14** | **S32-str/val.t (solved; session 39)** | **The multi-param restore of the inner `for (...) -> $string, \value` restored only env, not the local slot.** Initially diagnosed as "the subtest-closure read-coherence wall", but minimization showed a bare read in the inner `for ... -> \value` body, with no closure present, was already stale (`outer-read: 4_2 => -42`) = the diagnosis was wrong. Real cause = the inner for's sigilless `\value` multi-param **shares the same name = the same local slot as the outer `\value`**, and the loop-end `saved_multi_params` restore only restored the env binding, leaving the local slot at the final iteration value (-42) → with the reverse pull OFF, the next outer iteration re-evaluates the element list with a stale `value` | Added a **write-through to the local slot** to the multi-param restore loop (`exec_for_loop_body`, vm_control_ops.rs:1218) (if `find_local_slot(code, name)` exists, `locals[slot] = v`). Byte-identical with ON. pin=`t/multiframe-sigilless-for-rebind-coherence.t` (4) |

| **15** | **S03-junctions/autothreading.t (solved; session 39; CI surface)** | **Method autothreading over an invocant junction propagates captured-outer / `our` mutations only for the last eigenstate.** With `$junc.a` (`method a { $cnt++ }`), each eigenstate accumulates correctly in env (the threaded return value `any(0,1,0)` proves it), but the per-call pending writeback **carries only the last eigenstate's source** → when eigenstates write **different variables** (earlier=$cnt1, last=$cnt2), the earlier $cnt1 never reaches the caller local slot and stays stale (0). Previously recognized as the "method-invocant autothread `our`-var accumulation wall" (§34, deferred) and misclassified as "junctions flaky" in the local survey | Both junction loops of CallMethod / CallMethodMut (vm_call_method_ops.rs / vm_call_method_mut_ops.rs) return early before the normal post-dispatch reconcile, so added one `reconcile_locals_from_env_at_site(code)` after threading (env already holds the accumulated values of all eigenstates). Byte-identical with ON. pin=`t/junction-invocant-autothread-writeback-coherence.t` (6) |

| **15.5** | **Over-application regression of the val.t fix (solved; session 39; CI surface)** | #14's local-slot write-through applied to all of `multi_param_names` (including sigiled names) and ran for **every `for ... -> $a, $b` loop** → the slot of a sigiled param, which held a **live rw/element alias, got overwritten by the env-only restore value** → string/loop corruption (`expected bc got c`/`expected abecd got abec`). The parallel CI logs were unreadable (gh log truncation); **identified via the Test Summary of a local release `make roast`** | Restricted the write-through to **sigilless names only** (`!name.starts_with(['$','@','%','&'])`). The val.t bug is specific to the sigilless `\value`, so it is preserved; sigiled params go back to the (already coherent) env-only restore |
| **16** | **S32-io/IO-Socket-Async.t (solved; session 39) test 40 'listen tap is a Tap'** | `my $tap = do whenever $sup {…}` writes the tap directly to `env[target_var]` (4 sites in `run_whenever_with_value`) but never updates the caller local slot → **a subsequent read within the same react block** (`isa-ok $tap, Tap`) reads the stale slot (the `do` block's own result = a Supply). #3348's react-scope-end reconcile is too late for reads inside the block. This too was a deterministic OFF dependence misclassified as "IO-Socket-Async flaky" in the survey | Added `reconcile_locals_from_env_at_site(code)` to `exec_whenever_scope_op` (env_dirty already set). Byte-identical with ON. pin=`t/react-do-whenever-tap-coherence.t` (2) |

make test PASS (988 files / 9634 tests). **All 16/16 Stage 3 roast OFF dependences solved** (after the val.t
commit, CI surfaced autothreading.t, which had slipped out of the 14-item survey; a subsequent local release roast
surfaced IO-Socket-Async.t = junctions/IO-Socket-Async were not flaky but real dependences. Lesson: parallel CI
logs get truncated by gh and failures cannot be pinpointed → **the trailing Test Summary of a local release
`make roast` is authoritative**. set/baghash/mix fully PASS with OFF; sethash.t is a non-whitelisted plan-abort,
out of scope). **val.t was initially thought to be a multi-frame read-coherence wall, but in reality it was one
manifestation of the same single pattern as #1–13 (an interpreter-run/loop path writes a caller lexical to a shared
slot but the restore/reconcile does not restore the slot)**, and it was solved just by adding a local write-through
to the multi-param env-only restore (it was not an independent read-direction substrate). **Stage 3 (disabling
reverse-sync by default) is mergeable with all t/ + roast OFF dependences cleared.**

**Lesson**: the 14 items surfaced by Stage 3 were unrepresented by the t/ scan, but **13 of them were different
manifestations of a single pattern** (an interpreter-run op writes caller lexicals by name but never reconciles at
the call site), each solved just by adding `reconcile_locals_from_env_at_site(code)` at the op site
(byte-identical with ON). Only the remaining val.t was qualitatively different, as read-direction multi-frame
retention.

### 7.2 Stage 3 mechanism deletion — removal of the old reverse pull `sync_locals_from_env` (session 40)

With all 16/16 OFF dependences solved in §7.1, the default disabling of `MUTSU_REVERSE_SYNC` (#3349) passed the
full roast CI green. The old reverse pull thereby became **dead code in the default (= only shipping) build**
(`reverse_sync_disabled()` always true → `sync_locals_from_env` returns immediately). Actually deleted in this
session:

- **Deleted mechanisms**:
  - The body of `sync_locals_from_env` (the blanket env→locals reverse pull, `vm_env_helpers.rs`) and all its call
    sites (inside `ensure_locals_synced`; the closure tail at `vm_closure_dispatch.rs:633`; before smartmatch at
    `vm_comparison_ops.rs:1246`).
  - `reverse_sync_disabled()` and the opt-in `MUTSU_REVERSE_SYNC` escape hatch (`vm_stats.rs`). The A/B diagnostic
    has served its purpose.
  - Orphaned reverse-sync stats: `record_locals_pull` / `record_locals_pull_effective` / `record_stale_slot`, the
    statics `LOCALS_PULL{,_EFFECTIVE}` / `LOCALS_PULL_STALE_SLOTS` / `stale_slot_by_name`, and `dump()`'s
    reverse-sync lines and stale-slot histogram.
- **Behavioral identity**: the default build was already no-op'ing the reverse pull (after #3349), so the deletion
  does not change runtime behavior. `ensure_locals_synced` shrinks from "pull → flag clear" to "flag clear only"
  (the pull was already a no-op). The pre-smartmatch `if env_dirty { sync; clear }` shrinks to just
  `env_dirty = false`.
- **Kept mechanisms (= the new load-bearing set)**: the `env_dirty` flag **stays**. It functions as the
  `if env_dirty { reconcile }` gate of `reconcile_locals_from_env_at_site` /
  `drain_and_reconcile_after_cached_call`, and is also a perf optimization avoiding the O(locals) loop on pure
  calls (fib etc.) (not a replacement for the reverse pull, but the gate of the precise reconcile).
  `apply_pending_rw_writeback` / carrier logging / `note_caller_env_write` all continue as well.
- make test PASS (988 files / 9634 tests; count unchanged). diff = 4 files / +36 −191.
- **Next candidates**: the scattered rationale comments (~18 files saying "without the reverse
  `sync_locals_from_env` pull") are historically accurate (the pull no longer exists), so they were left unedited
  to keep focus. Simplifying `env_dirty` itself (whether the `saved_env_dirty` frame save is really needed;
  tidying the deep-copy paths) will be evaluated as a separate slice.

### 7.3 The wall of physically deleting `env_dirty` — multi-frame accumulation (session 40; demonstrated & reverted)

After removing the reverse pull in §7.2, attempted the deletion of `env_dirty` itself, and **confirmed it is
substrate-blocked**.

- **`env_dirty`'s current role**: no longer a correctness mechanism; it has been demoted to the
  `if env_dirty { reconcile }` gate of the precise reconcile
  (`reconcile_locals_from_env_at_site` / `drain_and_reconcile_after_cached_call`) = a **cheap perf optimization**
  avoiding the O(locals) loop on pure calls (fib).
- **The demonstrated wall**: removing the blanket reconcile from `drain_and_reconcile_after_cached_call` breaks
  4 subtests of `t/multi-frame-accumulation-coherence.t` (the cross-call accumulation of `via(); via()`).
  This blanket reconcile is **the only mechanism** holding up multi-frame accumulation; the precise single-frame
  `apply_pending_rw_writeback` cannot reach it.
- **Failure of the retention approach**: implemented retention in `apply_pending_rw_writeback` that carries a
  source over to the parent frame's drain (re-push) when it is not a local of the current frame — but **it could
  not fix the canonical case** (confirmed by actual tracing). Root cause = retention is a **consumption queue**,
  fragile to drain order: on the 2nd `via()` (cached fast path), bump-outer's drain runs at an intermediate point
  with `call_frames=0`, `code=via` (no acc local), and **drops the source before** reaching the owner (top; has
  the acc local). The blanket reconcile is an **idempotent per-site pull** that reads env each time and fixes the
  relevant slots, so it has no such drain-order problem and is robust. The retention (experiment) is reverted.
  - This re-confirms §6.3's "retention does not fix the canonical case" (the same wall even after §6.6's
    cached-path fix).
- **The root, and the next move**: as long as there are 2 places to store, some form of the marker
  "env was written by name → the slot may be stale" is necessary. Truly deleting `env_dirty` requires **making env
  a derived view of locals = single-store-ification**, which presupposes **env↔locals sharing the same container
  cell** (this document's §4-A outer cell sharing; an extension of `docs/container-identity.md` Phase 2;
  Sub-slice 1b). ∴ the `env_dirty` deletion is blocked on a single substrate (container cell sharing); it is not
  "a bit more cleanup" — starting the substrate is the rate-limiting step (reorganized into PLAN.md §1-A / §2-C /
  §2-E).
