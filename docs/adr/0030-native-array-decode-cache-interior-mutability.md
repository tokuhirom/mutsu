# ADR-0030: The native `array[T]` decode cache is a read-path cache, and needs field-level interior mutability — not `gc_contents_mut`

- **Status**: Proposed (design complete; implementation not started)
- **Date**: 2026-08-19
- **Deciders**: tokuhirom, Claude
- **Related**: [ADR-0013](0013-container-interior-mutability-cellvalue.md) (the `gc_contents_mut` chokepoint this decision deliberately does *not* reuse — see §3.1), [ADR-0015](0015-native-backed-container-storage-and-repr-bodies.md) (P3b introduced the cache this ADR repairs), [ADR-0001](0001-gc-strategy-and-phasing.md) §7 (layer 3c owns the residual cross-thread race), [todo/deep/native-array-storage-sync-unsound-interior-mutation.md](../../todo/deep/native-array-storage-sync-unsound-interior-mutation.md) (the root-cause analysis and repro)

> This ADR records why mutsu needs a **second** interior-mutability mechanism alongside ADR-0013's,
> which mechanism, and where it lives. It does not supersede ADR-0013 — it covers the one borrow
> shape ADR-0013 measured as UB and therefore excluded from `gc_contents_mut`'s contract.

---

## 1. Context

### 1.1 The defect

`ArrayData::sync_native_items(&self)` (`src/value/value_collections.rs`) mutates three plain,
non-interior-mutable fields — `items`, `native_dirty`, `native_snapshot` — through a
`self as *const Self as *mut Self` cast:

```rust
unsafe {
    let this = self as *const Self as *mut Self;
    std::ptr::addr_of_mut!((*this).items).write(decoded);
    std::ptr::addr_of_mut!((*this).native_dirty).write(false);
    std::ptr::addr_of_mut!((*this).native_snapshot).write(Some(bytes));
}
```

Writing through a `*mut T` derived from a live `&T` whose pointee contains no `UnsafeCell` is
undefined behavior under both Stacked and Tree Borrows. It survives `-O0` and miscompiles under
release optimization, because the `&self` signature licenses LLVM to treat `self.items` as
unchanged across the call and reuse a cached load.

The ticket root-caused it from `t/native-array-storage.t` subtest 6. A second probe written for
this ADR shows the blast radius is wider than that one subtest, and is a **lost write**, not only
a stale read:

```raku
my int @a = 10, 20, 30;
my $payload = nativecast(CArray[int64], nativecast(MVMArrayB, Pointer.new(@a.WHERE)).any);
$payload[2] = 99;   # C writes index 2; Raku has not read @a since
@a[0] = 7;          # a Raku-side write to a *different* index
say "{ @a[0] } { @a[2] }";
```

| build | result |
| --- | --- |
| `target/debug/mutsu` | `7 99` (correct) |
| `target/release/mutsu` | `7 30` — **the C write to index 2 is gone** |

(Rakudo itself segfaults on this program: a retained `VMArray` body pointer is only valid until
the container is resized, per ADR-0015 §5.3. Raku is therefore not a usable oracle for this shape;
the debug build is.)

### 1.2 Two independent bugs sit on top of each other

The probe fails for a reason the UB alone does not explain, and the fix must address both:

1. **The UB** (§1.1) — a stale read of `items` in release.
2. **`items_mut()` never syncs before marking dirty.**
   ```rust
   pub(crate) fn items_mut(&mut self) -> &mut Vec<Value> {
       self.native_dirty = self.native_storage.is_some();   // no sync_native_items() first
       &mut self.items
   }
   ```
   If C wrote the native buffer and Raku has not read the array since, `items` is stale. Marking
   dirty makes the *next* sync take the `encode` branch, writing that stale boxed cache back over
   the native bytes — silently discarding the C write. This is a plain logic bug, present in debug
   too, and it is why the probe loses index 2 rather than merely reporting it late.

### 1.3 Two more defects in the same function

Found while reading the same twenty lines; they belong in the same campaign because the rewrite
touches every one of them.

3. **The read path clones the whole native buffer on every access.** `sync_native_items` opens with
   `let current_bytes = node.bytes.clone();` purely to compare against the snapshot. Every
   `items()` — and therefore every `Deref` of a native array, i.e. every element read — allocates
   and copies the entire payload. The comparison needs no allocation at all
   (`self.native_snapshot.as_deref() == Some(node.bytes.as_slice())`).
4. **`ptr::write` leaks the superseded `Vec<Value>`.** `ptr::write` does not drop the old value, so
   every re-sync leaks a full element vector and its heap buffer, permanently. That leak is
   load-bearing by accident: it is the only reason an outstanding `&Vec<Value>` handed out by an
   earlier `items()` does not dangle. Any fix must replace it with something that keeps that
   property *deliberately* and reclaims the memory.

### 1.4 Why this is a new decision and not an application of ADR-0013

ADR-0013 §8 measured the aliasing shapes on the pinned nightly and recorded the rule its
`gc_contents_mut` call sites must obey:

> anything carried across the write must be a raw pointer or a handle-level operation, and a
> `Deref`'d `&T` must not be used after it

`items()` is *definitionally* the forbidden row:

```rust
pub(crate) fn items(&self) -> &Vec<Value> {
    self.sync_native_items();   // the write
    &self.items                 // a reference derived from the `&self` that predates it
}
```

The `&self` is Deref'd from `Gc<ArrayData>` by the caller, the write happens under it, and the
return value is derived from it afterwards. Routing the write through `gc_contents_mut` would fix
the *pointer derivation* and leave the *shape* untouched — still UB. ADR-0013's primitive is for
**a caller that holds the `Gc` handle performing a structural write and then dropping the `&mut`**;
this is **a read-path cache fill performed under a shared borrow the caller keeps using**. Those
are different problems, and Rust already names the second one's solution: an `UnsafeCell` at the
field, so the shared borrow is `SharedReadWrite` over those bytes and the write does not pop it.

---

## 2. Decision

**Move the native-storage lazy-sync state out of `ArrayData`'s flat field list into a single
optional, heap-allocated `NativeBacking` whose cache is behind a `SyncUnsafeCell`, and keep the
`items()` / `Deref` signatures exactly as they are today.**

```rust
/// `UnsafeCell` plus the `Sync` posture `Gc<ArrayData>` requires. Adding an
/// `UnsafeCell` field to `ArrayData` would otherwise make it `!Sync` and strip
/// `GcBox<ArrayData>` of the `Sync` that `unsafe impl<T: ?Sized + Sync> Sync for
/// GcBox<T>` (gc_ptr.rs) depends on. Same justification as that impl: the
/// cross-thread residue is unchanged and stays routed through the
/// `__mutsu_atomic_arr::` / `shared_vars` lanes (ADR-0001 layer 3c).
pub(crate) struct SyncUnsafeCell<T>(UnsafeCell<T>);
unsafe impl<T: Send> Sync for SyncUnsafeCell<T> {}

struct NativeBacking {
    /// The shared payload node. Unchanged from today's `native_storage`.
    node: Gc<BufData>,
    cache: SyncUnsafeCell<DecodeCache>,
}

struct DecodeCache {
    /// The live decode, plus every superseded one. A re-sync PUSHES a fresh
    /// box and never overwrites an existing slot, so a `&Vec<Value>` handed
    /// out by an earlier `items()` stays valid (§2.2).
    generations: Vec<Box<Vec<Value>>>,
    dirty: bool,
    snapshot: Option<Vec<u8>>,
}

pub struct ArrayData {
    /// Authoritative for an ordinary array. For a native-backed array this is
    /// the seed, superseded by `native`'s cache after the first re-sync.
    items: Vec<Value>,
    /// `None` for the overwhelming majority of arrays.
    native: Option<Box<NativeBacking>>,
    // ... value_type / key_type / declared_type / default / shape /
    //     initialized / descriptor_name unchanged
}
```

### 2.1 Ordinary arrays pay nothing

`items()` becomes:

```rust
pub(crate) fn items(&self) -> &Vec<Value> {
    match &self.native {
        None => &self.items,                 // identical codegen to today
        Some(nb) => nb.sync_and_borrow(&self.items),
    }
}
```

The non-native arm has no cell, no indirection, and no unsafe — it is the same field read the
current code performs, behind a discriminant check the current code *also* performs (today's
`sync_native_items` opens with `let Some(node) = &self.native_storage else { return }`). This
matters because `Deref for ArrayData` routes through `items()`, so this is not 37 call sites, it
is every array read in the interpreter. `ArrayData` also gets smaller: three fields
(`Option<Gc<_>>`, `bool`, `Option<Vec<u8>>`) collapse into one `Option<Box<_>>` pointer.

### 2.2 The generation graveyard is what makes it sound rather than merely well-typed

Field-level `UnsafeCell` legalizes the *write*. It does not, on its own, legalize a `&Vec<Value>`
handed out by an earlier `items()` and used after a later re-sync — overwriting the slot pops that
reference's tag exactly as it would without the cell.

That obligation cannot be discharged the way ADR-0013 discharged its own: ADR-0013 audited 62
enumerable call sites, whereas here `Deref` puts the obligation on *thousands* of `&*array_data`
sites. An unauditable obligation is not a resolution.

So the design removes the obligation instead of stating it: a re-sync **allocates a new
`Box<Vec<Value>>` and pushes it**, leaving every previously-handed-out slot untouched forever.
Outstanding references stay valid by construction. This is the deliberate, bounded, reclaimed
version of the accidental leak in §1.3-4.

**Growth is bounded by observed C writes, not by reads.** A re-sync only allocates when the
snapshot comparison fails, i.e. when the native bytes actually changed since the last decode. A
pure read loop over an unchanging native array allocates one generation total. `for ^1000 { say
@a[1] }` after a single C write allocates one.

**Reclamation is exact and needs no analysis.** Every `&mut self` method (`items_mut`,
`take_items`, `promote_native_storage`, `clear_native_storage`, and `Drop`) has a borrow-checker
proof that no shared borrow into the payload is live, so each one prunes the graveyard down to the
live generation. Any Raku-level write to the array therefore collects it.

### 2.3 `Clone` keeps today's semantics, exactly

`ArrayData` derives `Clone`, and `detach_shared_container` (`src/value/view.rs`) relies on it for
Raku `=` copy semantics. `NativeBacking` gets a **manual** `Clone`:

- `node` is cloned as a `Gc` **handle** — the copy shares the payload node, which is what the
  current derived `Clone` on `Option<Gc<BufData>>` already does. This was probed on both builds
  (`my int @d = @c; @d[0] = 555` leaves `@c[0] == 1`) and is benign today; preserving it exactly
  keeps this ADR's diff a soundness fix rather than a semantics change.
- The cache is **not** shared: the clone starts from a single fresh generation holding a deep copy
  of the live decode, with `dirty`/`snapshot` copied. The graveyard is *not* copied — a fresh
  `ArrayData` has handed out no references, so it has nothing to keep alive.

### 2.4 `Trace` must not sync, and must see every generation

`Trace for ArrayData` (`src/value/value_gc.rs:436`) reads the raw `self.items` field today, which
is correct and must stay correct: a sync at a collect safepoint would re-enter allocation from
inside the collector. Post-migration it visits the seed vector **and every retained generation** —
retired generations still hold `Value` edges the refcount accounting has counted, so skipping them
would under-report edges. `drop_gc_edges(&mut self)` clears all of them.

---

## 3. Options considered

| Option | Sound? | `Sync`? | Read-path cost | Blast radius | Verdict |
| --- | --- | --- | --- | --- | --- |
| Status quo (`*const Self as *mut Self`) | ✗ UB | n/a | 0 + a full buffer clone per read | — | Rejected — the defect |
| **1.** Propagate `&mut self` through `items()` | ✓ | ✓ | 0 | `Deref` makes it *thousands* of sites | Rejected — §3.2 |
| **2.** Route the write through `gc_contents_mut` | ✗ still UB | ✓ | 0 | ~37 sites re-signatured | Rejected — §3.1 |
| **3.** `RefCell` around the cache | ✓ | **✗** | guard per read | moderate | Rejected — §3.3 |
| **4.** `Mutex`/`RwLock` around the cache | ✓ | ✓ | guard per read + deadlock | moderate | Rejected — §3.3 |
| **5.** Atomic generation stamp + `Gc`-routed inner mutation | partial | ✓ | ~0 | large | Rejected — §3.4 |
| **6. Field-level `SyncUnsafeCell` + generation graveyard** | ✓ | ✓ | **0 for non-native** | one struct + its accessors | **Recommended** |

### 3.1 Why not the ADR-0013 chokepoint (the option the ticket suggested)

The ticket proposed extending `gc_contents_mut` to cover these fields, reasoning that reusing the
blessed primitive beats inventing a new one. That instinct is right in general and wrong here, for
two independent reasons.

**It does not fix the bug.** Per §1.4, ADR-0013 §8 *measured* this exact shape as UB under both
Stacked and Tree Borrows. `gc_contents_mut` makes the pointer's derivation valid; the hazard here
is the caller's live `&self`, which the primitive's own `# Safety` clause explicitly disclaims.
Adopting it would move the code under a safety comment that does not cover it — worse than the
status quo, because the UB would then look audited.

**It cannot be reached.** `sync_native_items` receives `&self`. Recovering the enclosing
`Gc<ArrayData>` from a `&ArrayData` is not possible, so the signature would have to change to a
free function over `&Gc<ArrayData>`, re-signaturing the 37 `items()` sites *and* `Deref` — which
cannot pass a handle at all, since `Deref::deref` takes `&self`. Losing `Deref` is the same
thousands-of-sites change as option 1.

The correct reading of ADR-0013 is therefore that it is **not** the universal container-mutation
primitive; it is the primitive for handle-holding structural writes. This ADR adds the sibling for
read-path cache fills. That is a genuine "1 operation = 1 implementation" tension and is the main
reason this decision is worth an ADR rather than a patch.

### 3.2 Why not propagate `&mut self` (the ticket's "shape 1")

The ticket sized this as 37 `items()` call sites. That undercounts it: `impl Deref for ArrayData`
returns `self.items()`, so *every* `&*array_data` in the interpreter is a read through this
chokepoint. Requiring `&mut` would mean either deleting `Deref` — ADR-0013 §6 records that `Deref`
is precisely what kept that campaign's blast radius at 51 sites instead of the whole codebase — or
threading `&mut` up call chains that legitimately hold only a shared borrow (comparison,
iteration, `Debug`/gist rendering, read-only dispatch). It also does not even express the
requirement: two *different* `&ArrayData` aliases of one node can both want a sync, which `&mut`
forbids and the container's identity semantics require.

### 3.3 Why not `RefCell` / `Mutex` / `RwLock`

`RefCell` is `!Sync`. `ArrayData` must be `Sync` for `unsafe impl<T: ?Sized + Sync> Sync for
GcBox<T>` to apply, and arrays cross `start`/thread-spawn boundaries under ADR-0001's model, so it
does not compile in position. It would also convert an aliasing question into runtime panics on a
re-entrant read — a failure mode that does not exist today.

`Mutex`/`RwLock` compile but re-import exactly what ADR-0013 §3 rejected: a guard on the read path
(the overwhelming majority of container touches) and the ADR-0001 §3-6 re-entrancy deadlock, where
a nested VM op wants the cache while an outer frame holds a read guard. And here it is worse than
expensive — it is type-incompatible: `Deref::deref` must return `&Vec<Value>`, and a guard cannot
outlive the `deref` call.

### 3.4 Why not an atomic generation stamp

Sound for `dirty` and for detecting staleness, but `items` is a `Vec<Value>` and `snapshot` an
`Option<Vec<u8>>` — neither is `Copy`, so an atomic stamp cannot carry them. It ends up needing a
cell for the payload anyway, plus a second synchronization concept to reason about. Strictly more
machinery for the same guarantee.

---

## 4. Migration plan

Ordered so that each step is independently reviewable and the correctness fixes land *before* the
representation change, keeping the release-only symptom bisectable.

| # | Step | Files | Blast radius |
| --- | --- | --- | --- |
| **1** | Fix §1.2: make `items_mut()` sync before setting `dirty`. Fix §1.3-3: drop the per-read `node.bytes.clone()` in favour of a slice comparison. Pin both with new subtests in `t/native-array-storage.t` (the §1.1 probe, and a read-only loop). | `value_collections.rs` | ~10 lines. Fixes the debug-observable half; release still fails on the UB. |
| **2** | Add `SyncUnsafeCell<T>` with its `unsafe impl Sync` and safety docs, plus unit tests in isolation. | new `src/value/sync_cell.rs` | Self-contained. |
| **3** | Introduce `NativeBacking` / `DecodeCache`; replace `native_storage` / `native_dirty` / `native_snapshot` with `native: Option<Box<NativeBacking>>`. Rewrite `items`, `items_mut`, `take_items`, `into_items`, `sync_native_items`, `promote_native_storage`, `clear_native_storage`, `native_storage_address`, `native_storage_node`, `native_repr_body_address`; manual `Clone`; update `Trace`/`drop_gc_edges` per §2.4. | `value/mod.rs`, `value_collections.rs`, `value_gc.rs` | Contained: the three fields are **private**, so nothing outside `src/value/` names them. The 37 `items()` and 62 `items_mut()` sites are **untouched** — signatures are preserved. Verified by grep: the only external readers go through `native_storage_node()` / `native_storage_address()` / `native_repr_body_address()`, which keep their signatures. |
| **4** | Add `src/value/native_cache_shapes.rs`, the Miri probe module (§5). | new file | Self-contained. |
| **5** | Widen the Miri CI filter to reach it (§5). | `.github/workflows/ci.yml` | One line + a comment. |

Step 1 alone makes the debug build correct and is worth landing even if the rest is deferred.

### Verification against the known repro

```sh
cargo build --release
timeout 30 target/release/mutsu t/native-array-storage.t          # 8/8, was failing subtest 6
timeout 30 target/release/mutsu tmp/native-cache-probe.p6         # "A: ... got 7 99"
```

Both must also pass on the debug build, and the release/debug outputs must be identical — that
equality *is* the acceptance criterion for a miscompilation bug, more than either build's absolute
result.

---

## 5. Miri: a probe module, and a CI filter that actually reaches it

Modeled directly on `src/gc/borrow_shapes.rs`, which exists for the same reason (the borrow checker
offers no protection at these sites, so pin the shapes instead of reasoning about them). New module
`src/value/native_cache_shapes.rs` must assert:

1. **The core shape** — a `&Vec<Value>` obtained from `items()`, then a C-side byte change, then a
   second `items()` that re-syncs, then **using the first reference**. This is UB today and must be
   clean after the fix. It is the test that would have caught the bug.
2. **Generation stability** — the first reference still reports the *old* decode after the re-sync
   (it points at a retired generation), while a freshly-taken one reports the new. This pins the
   semantics the graveyard chooses, so a later "optimization" that overwrites the slot fails here.
3. **Pruning is sound** — take a reference, drop it, call an `&mut self` method, assert the
   graveyard collapsed to one generation.
4. **`Sync` posture** — a compile-time assertion that `ArrayData: Sync` and `Gc<ArrayData>: Send +
   Sync` still hold.
5. **`Clone` independence** — a clone's cache is independent while its node is shared (§2.3).

**The CI job cannot see any of this today.** `ci.yml`'s Miri step filters with `--lib gc::`, a
substring match that reaches `gc::` and `value::value_gc::` but **not** `value::native_cache_shapes::`.
The *trigger* is already correct — `scripts/ci-docs-only.sh --gc-value` classifies `src/value/**`
as gc-value, so the job fires on these changes — it simply runs a filter that selects none of the
new tests. Step 5 adds the module to the filter (a second `--lib value::native_cache_shapes`
invocation in the leak-checked first step, which is where it belongs: these tests allocate only
what they assert on).

This is worth stating loudly because it is a silent gap, not a failure: without step 5 the job
goes green having executed nothing new.

Two inherited limits still apply and are not regressions: Miri falls back to permissive provenance
through the NaN-boxed `Value` layer (ADR-0013 §8), and FFI is dropped from the job — so the probes
simulate the C write with a direct write to `node.bytes`, exactly as
`nativecall_shape_raw_pointer_survives_a_later_deref` stands in for the real `nativecall` path.

---

## 6. Consequences

- **mutsu has two interior-mutability primitives, on purpose**, with a one-line rule for choosing:
  *holding the `Gc` handle and writing structurally → `gc_contents_mut`; filling a cache under a
  shared borrow the caller keeps using → `SyncUnsafeCell` at the field.* Both safety docs must name
  the other, or the next reader repeats the ticket's reasoning and reaches for the wrong one.
- **A real, unbounded memory leak is retired** (§1.3-4) and replaced by a bounded, reclaimed one.
- **The native-array read path loses a full-buffer allocation per element read** (§1.3-3) — the
  only performance-positive part of an otherwise soundness-motivated change.
- **`ArrayData` shrinks** by collapsing three fields into one `Option<Box<_>>`.
- **ADR-0015's P3b is completed rather than amended.** P3b shipped the `array[T]` node behind "the
  `ArrayData::items` accessor chokepoint"; the chokepoint was built, but built unsoundly. This ADR
  is that step's repair, not a new capability.
- **The residual cross-thread race is unchanged** and stays with ADR-0001 layer 3c: two OS threads
  racing on one native node are still governed by the `__mutsu_atomic_arr::` / `shared_vars` lane
  discipline, and nothing here mechanically checks it.
- **If rejected**: the UB stays, and with it a release-only divergence that is already reachable
  from two different Raku programs. Unlike ADR-0013's debt — "UB by the letter that the stress
  suites have never caught" — this one **has** been caught, by an ordinary optimized build. It is
  not a defensible holding pattern.

---

## 7. Open questions for the deciders

1. **Is the generation graveyard (§2.2) the right call, or is a stated-and-audited obligation
   acceptable after all?** The graveyard buys soundness-by-construction at the cost of retaining
   superseded decodes between `&mut` touches. The alternative — document the obligation and audit —
   is what ADR-0013 did, but it had 62 enumerable sites and this has `Deref`. **Recommendation:
   graveyard.**
2. **Should `NativeBacking::clone` keep sharing the `Gc<BufData>` node (§2.3)?** Preserved here to
   keep the diff a pure soundness fix, and probed benign on both builds. But a shared native buffer
   under Raku `=` copy semantics is a question ADR-0015 does not obviously answer, and it may be a
   latent bug that simply has no test. **Recommendation: preserve now, file separately.**
3. **Land step 1 (the two plain logic bugs) as its own PR ahead of the representation change?**
   It is small, fixes real lost writes in debug *and* release, and makes the remaining failure
   attributable to the UB alone. **Recommendation: yes.**

---

*This ADR is `Proposed`. If the mechanism judgment changes after implementation begins, supersede
it rather than rewriting it.*
