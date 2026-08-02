# ADR-0013: Container interior mutability — kill the `gc_contents_mut` provenance UB with a `GcCell` newtype

- **Status**: Accepted (2026-07-20 — mechanism 2b greenlit by tokuhirom. Open-Question resolutions recorded in §5. **Implementation refined at §7: the `UnsafeCell` lives in `GcBox.value`, not a `Gc<GcCell<Data>>` Value-layer wrapper** — same 2b mechanism, but it fixes all ~51 sites at the primitive with no `Value`-representation churn, so the phase-2/3 per-container migration and the `GcCell` newtype are dropped.)
- **Date**: 2026-07-20
- **Deciders**: tokuhirom, Claude
- **Related**: [ADR-0001](0001-gc-strategy-and-phasing.md) (layer 3a — this ADR fixes 3a's *unfinished* element-cell half), [ADR-0005](0005-nanbox-representation-encoding.md) (NaN-boxing; orthogonal — the 8B `Value` is unchanged), [docs/gc-contents-mut-inventory.md](../gc-contents-mut-inventory.md) (Step 1 inventory — the 54-site classification this ADR acts on), PLAN.md §2.1 (GC soundness tail)

> This ADR records the mechanism selection for removing the last GC-soundness debt: the deliberate
> aliased in-place container writes (`gc::gc_contents_mut`) that preserve Raku container identity but
> do so through a raw-pointer cast that is Rust UB. It does **not** supersede ADR-0001 — it fills in
> the one piece of ADR-0001's "layer 3a" that shipped incomplete (the cycle collector landed; the
> Track-B element-cell-ification that would make identity-preserving mutation *sound* did not).

---

## 1. Context

### 1.1 What shipped, and what did not

ADR-0001 defined **layer 3a** as a single campaign: *Track B (element cell-ification) + cycle
collector = one-shot `Arc → Gc<T>` replacement of the container kinds*. In practice the campaign
shipped in two halves and only one is complete:

- ✅ **`Arc → Gc<T>` migration** — done (79 sites; container kinds are `Gc<T>` now).
- ✅ **Bacon-Rajan cycle collector** — done, default on (ADR-0001 §3-8, ADR-0003 trigger). **This
  closed the leak that made GC table-stakes.**
- ❌ **Track B element cell-ification / sound interior mutability** — *not* done. Identity-preserving
  in-place container mutation is still implemented with a raw-pointer cast (`gc::gc_contents_mut`),
  exactly the pre-GC mechanism, now spelled `Gc::as_ptr(gc) as *mut T` instead of
  `Arc::as_ptr(arc) as *mut T`.

So the "one campaign" premise is already spent: the collector half is in `main`, and this ADR is
about the remaining half in isolation.

### 1.2 The debt, precisely (Step 1 inventory)

`docs/gc-contents-mut-inventory.md` read and classified **every** production site — **54 sites / 20
files**:

- **(a) provably-unique = 3** — cyclic-structure construction on a freshly-created node
  (`vm_var_assign_ops.rs`). Documented with `debug_assert` (Step 2) and machine-checked by
  `Gc::verify_unique_for_aliased_mut` (Step 4). **Sound; not this ADR's target.**
- **(b) make_mut-COW-coverable = 0** — every guarded site already routes the unique case to
  `make_mut` and reserves `gc_contents_mut` strictly for the shared+identity case. **COW retires
  nothing.**
- **(c) needs first-class cell = 51** — identity-required writes on a genuinely-shareable node.
  **These 51 are this ADR's target.**

The (c) sites are legitimate: a Raku mutation through one alias of an `@a` / `%h` must be visible
through *every* holder of the same container (`:=` binds, captures, the env mirror). `Gc::get_mut`
returns `None` the moment the node is aliased (which is exactly when the shared write is needed) and
`Gc::make_mut` clones (severing the alias), so **an in-place write through the shared node is
fundamental, not an optimization** (`value/aliased_mut.rs` module docs).

### 1.3 What is actually unsound — two distinct problems, very different sizes

The single `gc_contents_mut` primitive (`&mut *(Gc::as_ptr(gc) as *mut T)`) carries **two** separate
unsoundnesses, and conflating them has made the debt look scarier than it is:

1. **Provenance UB (broad, every run, Miri-detectable).** Deriving a `&mut T` from a `*const T`
   obtained from a shared `&` violates Stacked/Tree Borrows **even single-threaded**. This is present
   at all 51 sites on every execution. It is the dominant problem.
2. **Cross-thread data race (narrow, partly already mitigated).** A genuine race exists only when the
   *same node* is structurally mutated from two OS threads. mutsu already routes cross-thread shared
   container mutation through synchronized lanes — the `__mutsu_atomic_arr::` / `__mutsu_atomic_hash::`
   store under the `shared_vars` write lock (`runtime/builtins_atomic_shared.rs`) — so the
   `gc_contents_mut` sites are overwhelmingly **same-thread aliased writes**, not live cross-thread
   races. gc-stress + the whitelisted S17 suite have not surfaced a race here.

**Consequence for the decision:** the fix that matters most, and is cheapest, is the one that kills
(1). (2) is a smaller, separable question that belongs with the owning-thread / biased-refcount work
(ADR-0001 layer 3c), not on the critical path of this ADR.

### 1.4 Why this is deferred-but-worth-an-ADR

Deferred (user decision 2026-07-20) because it is a **large, high-blast-radius change for a
soundness-only payoff** (no new feature, no correctness bug reachable from Raku today — 3a already
closed the leak). But it is the *only* path that removes the UB, and "UB by the letter that has
passed the stress suites so far" is not a resting place. This ADR frames the mechanism so the
campaign can start the moment it is greenlit, and so the framing survives the wait.

---

## 2. Decision (recommended direction)

Introduce a **`GcCell<T>` interior-mutability newtype** and migrate the container kinds from
`Gc<ContainerData>` to `Gc<GcCell<ContainerData>>`, converting the 51 (c) sites from a raw-pointer
cast to a **sound `UnsafeCell`-based aliased borrow**.

```rust
/// Interior mutability for a GC-managed container payload. The aliased,
/// identity-preserving in-place write that Raku container semantics require
/// goes through `UnsafeCell::get()`, so the resulting `&mut` has valid
/// provenance even while shared `&` borrows into the same node exist — the one
/// thing `Gc::as_ptr(gc) as *mut T` cannot give. Reads stay as bare `&T`
/// (Deref preserved), so the hot read path (`.iter`/`.len`/index) pays nothing.
pub struct GcCell<T> {
    value: UnsafeCell<T>,
    // Debug/verify-only overlap detector (RefCell-style, atomic so it is Sync).
    // Release builds carry only the UnsafeCell.
    #[cfg(any(debug_assertions, feature = "gc-verify"))]
    borrow: AtomicIsize,
}
```

`gc::gc_contents_mut` is replaced by `GcCell::borrow_mut_aliased(&self) -> &mut T` (`&mut *self.value.get()`).

**This is the mechanism decision. The cross-thread race (§1.3 problem 2) is explicitly left to the
lane discipline + ADR-0001 layer 3c (see Open Questions §5).** `unsafe impl Sync for GcCell` is
justified by exactly the same discipline that already justifies today's `Gc<ArrayData>: Sync` — the
difference is that provenance becomes valid, so the residue shrinks from "UB + race" to "race,
narrow, lane-governed".

### Why this direction

- **It kills the broad problem (provenance UB) at all 51 sites** and turns the primitive from
  "unsound by construction" into "sound single-threaded, race-governed cross-thread" — the same
  soundness posture as any `UnsafeCell`-based container in std.
- **~Zero read-path cost.** Reads remain `&T` through `Deref`; only the 51 mutation sites change.
  Contrast a lock (§3 option 2a), which taxes every `.iter`/`.len`/index and re-introduces the
  re-entrancy deadlock ADR-0001 §3-6 warns about.
- **Miri becomes an acceptance gate.** After migration, a Miri run over the container tests must be
  clean — a concrete, machine-checkable definition of done that the raw-pointer version can never
  pass.
- **The Step 4 hook already guards the (a) sites**; the same `Arc`-vs-`header` accounting invariant
  extends naturally to the migrated (c) sites.

---

## 3. Options considered

### Mechanism for the provenance fix

| Option | Read-path cost | Deadlock risk | Fixes provenance UB | Verdict |
|---|---|---|---|---|
| **Keep `Gc::as_ptr as *mut` (status quo)** | 0 | — | ✗ (UB) | Rejected — the debt itself |
| **2a. Per-container `RwLock`/`Mutex` (`Gc<RwLock<T>>`)** | High (every read guards; `Deref` to `Vec` lost) | **High** (re-entrant write while a read guard is held — the ADR-0001 §3-6 landmine) | ✓ | Rejected-leaning (perf + deadlock) |
| **2b. `GcCell` = `UnsafeCell<T>` + debug/verify atomic borrow flag** | 0 | None (no lock) | ✓ | **Recommended** |
| **2c. Element-level cells only (`Vec<ContainerRef>` — "Track B proper")** | ~0 for element writes | None | Partial | Complementary, not a substitute (see below) |

- **2a** is the "obvious" fix and the reason to write the ADR is to reject it deliberately: locking
  the whole container makes reads — the overwhelming majority of container touches — pay a guard, and
  it re-opens the re-entrancy deadlock (a nested VM op taking a write guard on a node an outer frame
  already read-guards). mutsu's identity-write pattern is *intentionally* re-entrant/aliased; a
  discipline that panics or deadlocks on that (RefCell / RwLock) fights the design.
- **2c** (make each element a first-class cell) is real and desirable, but it only covers *element*
  writes (`@a[i] = x`); **structural** mutation (push/pop/insert/remove/grow — which changes the
  Vec/HashMap length) still needs container-level interior mutability. So 2c reduces the (c) site
  count but cannot reach zero on its own. It layers cleanly on top of 2b and can be pursued
  incrementally afterwards (or folded in — Open Question §5).

### Cross-thread race (problem §1.3-2)

| Option | Cost | Verdict |
|---|---|---|
| **Per-node lock now** | Read tax + deadlock (== option 2a) | Rejected — solves a narrow problem with a broad tax |
| **Keep the `__mutsu_atomic_arr::` / `shared_vars` lane discipline + defer to layer 3c** | 0 | **Recommended** — the race is already lane-governed; formalize + audit it in 3c |
| **`unsafe impl Sync` with a documented owning-thread contract** | 0 | Adopted as the interim justification (same posture as today, minus the provenance UB) |

---

## 4. Phasing

1. **`GcCell<T>` newtype** — `UnsafeCell` + `Deref<Target=T>` (reads), `borrow_mut_aliased` (the 51
   sites), debug/verify atomic overlap detector, `unsafe impl Send + Sync` with the documented
   contract. Unit-tested in isolation (aliased write visible through a clone; overlap flag trips in
   debug).
2. **Migrate the container kinds** `Array` / `Hash` / `Set` / `Bag` / `Mix` / `Pair` / `Instance` /
   `Sub` / `ContainerRef` / `LazyList` payloads: `Gc<Data>` → `Gc<GcCell<Data>>`. Reads unchanged via
   `Deref`. (Scope-per-slice vs all-at-once is Open Question §5.)
3. **Rewrite the 51 (c) sites** `gc_contents_mut(&gc)` → `gc.borrow_mut_aliased()`. Delete
   `gc::gc_contents_mut` and the `aliased_mut` shadow once the last site is gone.
4. **Miri gate** — a `cargo miri test` pass over the container/GC test subset must be clean (no
   Stacked/Tree Borrows violation). This is the definition of done; add it to CI as a non-blocking
   informational job first, then blocking once green. **Toolchain note:** Miri needs a nightly whose
   feature set matches the crate's stabilized-feature usage (e.g. `if_let_guard`); pin a recent
   nightly in the CI job. Deferred until phase 3 makes GcCell live on a real execution path (Miri over
   the phase-1 unit tests only re-proves std's `UnsafeCell` guarantee).
5. **Keep gc-stress + S17 green** throughout; the Step 4 `verify_unique_for_aliased_mut` accounting
   invariant extends to the migrated sites.

---

## 5. Open questions (the real forks for the deciders)

1. **Mechanism: confirm 2b.** ✅ **RESOLVED (2026-07-20): 2b.** `GcCell` = `UnsafeCell` + debug
   borrow-flag. 2a (whole-container lock) rejected on the record for the read-tax + the ADR-0001
   §3-6 re-entrancy deadlock.
2. **Cross-thread race: defer to layer 3c, or lock the genuinely-shared nodes now?** ✅ **RESOLVED:
   defer to layer 3c.** The race is narrow and lane-governed; a per-node lock re-introduces the exact
   read-tax + deadlock this ADR avoids. Revisit only if gc-stress/S17 surfaces an actual race.
3. **Element cells (2c / Track B proper): fold into this campaign or defer?** ✅ **RESOLVED: defer.**
   Land 2b first (it retires the UB), then pursue 2c incrementally where it measurably reduces
   container-level structural sites.
4. **Scope: all 9 container kinds at once, or Array/Hash first?** ✅ **RESOLVED: Array/Hash first.**
   They cover the large majority of the 51 sites; a slice-by-kind rollout keeps each PR reviewable and
   each Miri run bisectable. The setbagmix/instance/sub tail follows.
5. **`GcCell` vs reusing an existing crate.** ✅ **RESOLVED: own `Send + Sync` cell.** rust-gc's
   `GcCell` is `!Send` (ADR-0001 §6 rejected the rust-gc family for exactly this); we do not adopt a
   crate.

---

## 6. Consequences

- **`gc::gc_contents_mut` and `value/aliased_mut.rs` are deleted** at the end of the campaign; the
  "known unsoundness" module docs go away because the unsoundness is gone (for the provenance half).
- **Miri joins the toolbox** for GC/container work — a lasting acceptance gate, not just for this
  campaign.
- **`ArrayData`/`HashData` field access stays unchanged** for the ~thousands of read sites (`Deref`),
  so the blast radius is the 51 write sites + the type signatures, not the whole codebase.
- **The 8B NaN-boxed `Value` is unchanged** (ADR-0005): a container is still a single `Gc` pointer
  tag; only the pointee type gains a `GcCell` wrapper.
- **ADR-0001's "layer 3a" is finally complete** once this lands — the element-cell/interior-mutability
  half that shipped incomplete is closed, and the GC-soundness tail (PLAN §2.1) is done except for
  the layer-3c cross-thread-race formalization.
- **If rejected / indefinitely deferred:** the `gc_contents_mut` UB stays, mitigated (not removed) by
  the Step 2 asserts + Step 4 verify hook + the single audited choke point. That is a conscious
  "accept the letter-of-the-law UB while the stress suites stay green" posture — acceptable only as a
  holding pattern, not a resolution.

---

## 7. Refinement (2026-07-20): place the `UnsafeCell` in `GcBox`, not a Value-layer wrapper

While starting §4 phase 1, a materially better *placement* of the same 2b mechanism surfaced. The
decision (2b = `UnsafeCell` interior mutability, defer the cross-thread race) is unchanged; only where
the cell lives changes — so this is a refinement recorded here, not a superseding ADR.

**What changed.** Instead of wrapping each container payload as `Gc<GcCell<Data>>` at the `Value`
layer, put the `UnsafeCell` one level down, in the primitive's node box:

```rust
struct GcBox<T: ?Sized> { header: GcHeader, value: UnsafeCell<T> }   // was: value: T
```

`Gc::as_ptr` / `gc_contents_mut` / `make_mut` / `get_mut` project through the payload with
`UnsafeCell::raw_get` (no intermediate reference), and `Gc`'s `Deref` reads through `UnsafeCell::get`.
That makes **every** `Gc<T>` interior-mutable at the primitive, so the ~51 `gc_contents_mut` sites
become provenance-sound *with no change to their call sites at all* — `gc_contents_mut`'s body is
unchanged; only the pointer it derives now carries interior-mutable provenance.

**Why it is better than the Value-layer wrapper.**

- **Blast radius collapses to ~one file** (`src/gc/gc_ptr.rs`). The wrapper plan had to rewrite
  `ValueRepr` / `ValueView` / the nanbox encode/decode/peek / the 34 `Gc<ArrayData>`/`Gc<HashData>`
  type references / the container constructors / the 51 write sites. The GcBox placement touches none
  of them — the `Value` representation, the nanbox 8B encoding, and all 51 call sites are byte-for-byte
  unchanged.
- **One mechanism for all container kinds at once** (Array/Hash *and* Set/Bag/Mix/Instance/Sub/…), so
  the per-kind phasing (Open Question §5.4) is moot — there is nothing to roll out kind-by-kind.
- **Feasibility verified**: `UnsafeCell<T>` coerce-unsizes, so `GcBox<T>` → `GcBox<dyn Trace>` (the
  erased `ErasedGc`) still works; `UnsafeCell` is `#[repr(transparent)]`, so box layout and `size_of`
  are unchanged.

**Consequences of the refinement.**

- **The `GcCell` newtype (phase-1 PR #4956) is removed** — the GcBox placement subsumes it; keeping
  both would violate "1 operation = 1 implementation".
- **The phasing (§4) collapses**: there is no phase-2/3 per-container migration. The remaining work is
  the primitive change (this PR) + the Miri gate (§4 phase 4, still deferred until a nightly matching
  the crate's stabilized features is pinned in CI).
- **`Sync` justification moves to `GcBox`**: `unsafe impl<T: ?Sized + Sync> Sync for GcBox<T>`, the
  same posture the pre-UnsafeCell `GcBox<dyn Trace>` already had (its `dyn Trace: Sync`); the interior
  mutability adds no cross-thread write that the old raw-pointer `gc_contents_mut` did not already have.
- Everything else in this ADR (the two-unsoundness split §1.3, the lock rejection §3, the layer-3c
  deferral) stands unchanged.

---

---

## 8. Implementation status (added 2026-08-02)

§4's phasing collapsed per §7, so the campaign is now two items — one shipped, one not. Recorded
here because the ADR previously gave no way to tell them apart.

**✅ Shipped — the primitive change.** `GcBox` stores its payload as `value: UnsafeCell<T>`
(`src/gc/gc_ptr.rs:166`); `Gc::as_ptr` projects through it with `UnsafeCell::raw_get` and no
intermediate reference (`gc_ptr.rs:198`); `Gc`'s `Deref` reads through `UnsafeCell::get`;
`unsafe impl<T: ?Sized + Sync> Sync for GcBox<T>` restores the pre-cell `Sync` posture
(`gc_ptr.rs:169-176`). `gc_contents_mut` (`gc_ptr.rs:774`) is byte-identical in body but now
derives its `&mut` from an interior-mutable pointer, so **problem §1.3-1 (provenance UB) is
fixed at every call site at once**, with the `Value` representation, the NaN-box encoding and
all call sites untouched — exactly as §7 predicted. Site count as of 2026-08-02: **59 across 24
files** (the inventory's 54/20 has drifted up as new mutation paths reuse the primitive; the
drift is expected and harmless now that the primitive is sound, but it means the number in
`docs/gc-contents-mut-inventory.md` is a snapshot, not a budget).

**⚠️ Correction (2026-08-02, same day): "fixed at every call site at once" is wrong by one
call site.** The `UnsafeCell`-in-`GcBox` fix covers every `Gc`-managed container, which is all
of them **except `Mixin`**: `ValueRepr::Mixin(Arc<Value>, Arc<HashMap<String, Value>>)`
(`src/value/mod.rs`) never migrated to the GC, and `$type.^set_name(...)` writes its overrides
map in place through `arc_contents_mut` (`src/runtime/methods_classhow_dispatch.rs:109`). An
`Arc` payload has no `UnsafeCell`, so that one site is still the original provenance violation.
`aliased_mut.rs` compounded the error by documenting `arc_contents_mut` as "currently unused".
Tracked in `todo/tickets/mixin-overrides-aliased-write-is-still-arc.md`; found by writing the
Miri gate below, which is exactly the kind of thing the gate exists to surface.

**✅ Shipped, blocking — §4 phase 4, the Miri gate.** ci.yml's `miri` job runs
`cargo miri test --no-default-features --features native --lib gc::` (JIT and FFI dropped —
Miri cannot execute either) on a **pinned** nightly: Miri is nightly-only, and an unpinned
nightly would turn a toolchain update into what looks like a soundness regression. It is a
**required status check**, not an informational one — on a repository where every PR
auto-merges, an informational job is a job nobody reads. Cost is controlled by the trigger, not
by weakening the check: the job is gated on a `gc-value` classification (`scripts/ci-docs-only.sh
--gc-value`) so it runs only for changes under `src/gc/**` / `src/value/**`, ~8% of merges
measured over the last 300, and is *skipped* (which branch protection counts as success)
otherwise.

**Its current reach is the primitive, not yet the VM.** All 33 `gc::` unit tests are Miri-clean,
including `gc_contents_mut_writes_through_a_shared_node`, which is the exact aliased-write shape
this ADR made sound. The interpreter-level half — `gc::soundness_smoke`, four tiny Raku programs
run through a real `Interpreter` so Miri watches the VM take an aliased `&mut` into a shared node
while other handles are live — exists but is `#[cfg_attr(miri, ignore)]`: `Interpreter::new()`
shells out to `uname`/`hostname` while building `$*DISTRO`/`$*KERNEL`, and Miri cannot spawn a
process. `todo/tickets/magic-vars-should-be-built-lazily.md` unblocks it; dropping that
`cfg_attr` is what answers §4's warning that a primitive-only run "mostly re-proves std's
`UnsafeCell` guarantee".

**Two limits to state honestly.** (1) Miri reports `integer-to-pointer cast` on the NaN-boxed
`Value` (ADR-0005), so it falls back to permissive provenance for pointers recovered from the
box: checking is precise for the typed `Gc<T>` paths the unit tests exercise and best-effort
through the `Value` layer. `-Zmiri-strict-provenance` is not available to us for the same reason.
(2) One race-hammer test (`concurrent_buffer_and_drain_never_wraps_the_approx_count`, 200k
allocations across four threads) is ignored under Miri because it would not finish; it defends
counter ordering, not provenance, and gc-stress still runs it natively.

This ADR is therefore closed **except** for the `Mixin` correction above, which the gate cannot
see today (no test in the subset reaches `^set_name`).

**Deferred by decision, not by omission** — problem §1.3-2, the narrow cross-thread race on a
genuinely shared node, remains routed to ADR-0001 layer 3c (§5 open question 2). The safety
contract on `gc_contents_mut` states the obligation ("concurrent structural mutation from
another thread remains routed through the synchronized shared-store lanes"), and nothing
mechanically checks it.

**Documentation debt this ADR created.** `src/value/aliased_mut.rs`'s module header still
carries a "⚠️ Known unsoundness (tracked, not removed here)" section describing the
`Arc::as_ptr` provenance violation as live and naming Track B as the future fix. Both
statements are false since this ADR landed. That header is the first thing a reader looking
for the current soundness posture finds; correcting it is part of closing this ADR.

---

*This ADR is Accepted (mechanism 2b; placement per §7; status per §8). If the mechanism judgment
changes later, supersede this ADR rather than rewriting it.*
