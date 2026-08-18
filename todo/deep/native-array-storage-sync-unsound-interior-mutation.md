# `sync_native_items`'s `&self`-based interior mutation is the release-only miscompilation, root-caused

Supersedes `todo/tickets/native-array-storage-write-through-lost-in-release.md`
(same repro, `t/native-array-storage.t` subtest 6 fails deterministically in
release builds only, passes in debug). That ticket's "scope note" flagged the
root cause as unfound and requested a `rust-gdb`-on-release-binary
investigation. This entry replaces the open question with a precise,
high-confidence root cause found via targeted probes (below) plus static
analysis, and records why the fix is out of scope for a quick ticket.

## Isolating the failure to exactly one step

Added a differential probe reading the SAME payload through the raw NativeCall
pointer immediately after the C-side write, before touching `@a[1]` at all:

```raku
$payload[1] = 42;
say $payload[1];   # 42 — the raw write DID land correctly
say @a[1];         # 20 — Raku's own array read does NOT see it
```

Both lines run against the same `~/target/release/mutsu` binary. This proves:

- The write itself is correct — `write_field` (`src/runtime/cstruct_layout.rs:531`,
  `native_carray_element_assign`) writes to the right address, and that
  address genuinely aliases the array's live backing storage (a raw pointer
  read immediately confirms `42`).
- The bug is entirely on the READ side: `@a[1]`'s indexing path fails to
  observe the change that objectively already happened in memory.

## Confirming the read path is reached, then finding what's unsound in it

`@a[1]` resolves through `exec_index_op_with_positional`
(`src/vm/vm_var_index_ops.rs`) → `resolve_array_entry`
(`src/vm/vm_var_ops.rs:123`) → `items.get(idx)` on a `&Gc<ArrayData>`, which
derefs through `Gc`'s `Deref` to `&ArrayData`, then through `ArrayData`'s own
`Deref<Target = Vec<Value>>` impl (`src/value/value_collections.rs:298-303`):

```rust
impl std::ops::Deref for ArrayData {
    type Target = Vec<Value>;
    fn deref(&self) -> &Vec<Value> {
        self.items()   // <- the representation chokepoint, calls sync_native_items()
    }
}
```

So the sync mechanism IS on the read path — this isn't a "the fast path
bypasses the chokepoint entirely" bug. The chokepoint itself is where the
unsoundness lives. `items()` and `sync_native_items` (both `src/value/value_collections.rs`):

```rust
pub(crate) fn items(&self) -> &Vec<Value> {
    self.sync_native_items();
    &self.items
}

fn sync_native_items(&self) {
    let Some(node) = &self.native_storage else { return };
    let current_bytes = node.bytes.clone();
    if !self.native_dirty && self.native_snapshot.as_ref() == Some(&current_bytes) {
        return;
    }
    let (bytes, decoded) = /* re-decode from node.bytes */ ...;
    unsafe {
        let data = crate::gc::gc_contents_mut(node);
        data.bytes.clear();
        data.bytes.extend_from_slice(&bytes);
    }
    unsafe {
        let this = self as *const Self as *mut Self;
        std::ptr::addr_of_mut!((*this).items).write(decoded);
        std::ptr::addr_of_mut!((*this).native_dirty).write(false);
        std::ptr::addr_of_mut!((*this).native_snapshot).write(Some(bytes));
    }
}
```

**`sync_native_items` takes `&self`, not `&mut self`, but mutates `self.items`
/ `self.native_dirty` / `self.native_snapshot` by casting `self as *const Self
as *mut Self` and writing through the raw pointer.** None of `ArrayData`'s
fields are `UnsafeCell`-wrapped (`items: Vec<Value>`, `native_dirty: bool`,
`native_snapshot: Option<Vec<u8>>` are all plain fields, per the struct
definition in `src/value/mod.rs:1198-1205`). This is the textbook unsound
"interior mutation via a `*const T as *mut T` cast on a plain, non-`Cell`-
wrapped field" pattern: it compiles and often *works* under `-O0`/debug (no
caching/reordering of the field read across the call), but is genuine
undefined behavior per Rust's aliasing model (both Stacked and Tree Borrows
reject writing through a `*mut T` derived from a live `&T` when the pointee
isn't declared as interior-mutable) — LLVM is free to assume `self.items`'s
value is unchanged across the `sync_native_items(&self)` call site (since the
signature promises no mutation through `&self`) and cache/reuse an earlier
read, which is exactly consistent with the observed symptom: the freshly
`Deref`'d `&Vec<Value>` still reflects the OLD decoded value (`20`) even
though the raw memory (and the re-decoded `data.bytes`/`node`) genuinely
changed. This class of bug is well-known to manifest ONLY under real
optimization (inlining, GVN/CSE, unaffected by codegen changes that don't
reorder loads) — matching "debug passes 8/8, release fails deterministically
on subtest 6, 3/3 runs" exactly.

The `unsafe { gc_contents_mut(node) }` call two lines above (writing into the
`Gc<BufData>` node itself) is a SEPARATE, already-audited pattern — see its
own extensive safety doc at `src/gc/gc_ptr.rs:786` (ADR-0013's blessed
interior-mutability chokepoint for `Gc<T>`, which DOES go through the
`UnsafeCell`-backed `Gc::as_ptr` provenance). The bug is specifically the
SECOND `unsafe` block, mutating the surrounding `ArrayData` struct itself
(not `Gc`-wrapped at this level — it's the payload the `Gc<ArrayData>`
around the whole array wraps, but `sync_native_items` receives a plain
`&self`/`&ArrayData`, not a `Gc<ArrayData>` it could route through the same
blessed chokepoint).

## Why this is `todo/deep`, not a `todo/tickets` slice

A sound fix needs one of two shapes, both substantial:

1. **Propagate `&mut self` through the read chokepoint.** `items()` is called
   at 37 sites across the codebase (`grep -rn '\.items()' src/ | wc -l`), many
   through contexts that only hold `&ArrayData`/`&Gc<ArrayData>` (comparison,
   iteration, `Debug`/gist rendering, read-only dispatch) where a `&mut`
   either isn't available or would need a much larger threading change up
   every call chain — not a local fix.
2. **Give the three lazily-synced fields real interior mutability** (e.g. a
   `Cell`/`RefCell`/atomics-based wrapper solely for `items`/`native_dirty`/
   `native_snapshot`, keeping `items()`'s `&self` signature). This needs
   careful design: `ArrayData` derives `Clone` (used throughout the codebase's
   COW/reassignment paths — `RefCell::clone()` deep-copies the interior,
   which may or may not be the wanted semantics here) and native arrays are
   shared across `start`/thread-spawn boundaries per this codebase's
   concurrency model (ADR-0001 layer 3c), where a plain `RefCell` is not
   `Sync` and would not compile in a cross-thread-shared context, and a
   runtime-borrow-checked `RefCell` changes panics-on-conflicting-access
   semantics that don't exist today. The right primitive (`Cell` requires
   `Copy` — `Vec<Value>`/`Option<Vec<u8>>` aren't; an atomic-generation-stamp
   plus a genuinely GC-routed inner mutation; or moving the whole lazily-
   synced trio behind the SAME `Gc`-level `UnsafeCell` chokepoint
   `gc_contents_mut` already uses for `node`, requiring `ArrayData` itself to
   always be accessed via a `Gc<ArrayData>` handle at this call site rather
   than a bare `&ArrayData`) needs a real design decision, not a drive-by
   patch.

Given ADR-0013's own §7-8 history (the `GcBox`/`UnsafeCell` refinement was
exactly this class of problem, done carefully with Miri-pinned tests — see
`gc_contents_mut`'s own safety doc, `src/gc/borrow_shapes.rs`), the
appropriate fix here should probably extend that SAME blessed mechanism to
cover `ArrayData`'s lazy-sync fields, rather than inventing a new ad hoc
interior-mutability primitive. That is real architectural work, not a
point fix.

## Severity

Low-to-moderate in practical surface (this specific write-through-native-
storage shape is a narrow NativeCall pattern — `.WHERE` + `nativecast` +
direct pointer write into a Raku-native array's backing store — not common
user code), but the underlying UNDEFINED BEHAVIOR itself (an unsound `&self`
+ raw-pointer-cast mutation, confirmed via Miri-class reasoning even though
not yet run through Miri itself for this exact function — CI's `miri` job
should be checked/extended to cover this) is a real soundness bug independent
of whether any particular roast test currently exercises it. A future,
unrelated optimizer/inlining change elsewhere in the interpreter could make
this manifest differently (or newly) without any change to this file.

## Repro

```sh
cargo build --release
timeout 30 target/release/mutsu t/native-array-storage.t
# subtest 6 fails deterministically (3/3 runs); debug build passes all 8
```

Affected: `src/value/value_collections.rs` (`ArrayData::items`,
`items_mut`, `sync_native_items`), `src/value/mod.rs` (`ArrayData` struct
definition), `src/gc/gc_ptr.rs` (`gc_contents_mut` — the existing sound
chokepoint this likely needs to route through instead).

---

## Proposed design (2026-08-19)

Designed out in full as **[ADR-0030](../../docs/adr/0030-native-array-decode-cache-interior-mutability.md)**
(`Proposed`). That ADR is the authority; this section is the summary and the
corrections it makes to the analysis above.

### Correction 1: `gc_contents_mut` is the wrong fix, and the ticket's closing paragraph is wrong to suggest it

The "Why this is `todo/deep`" section ends by proposing that the fix "should
probably extend that SAME blessed mechanism". It should not, for two independent
reasons:

- **It would not fix the bug.** ADR-0013 §8 *measured* this shape on the pinned
  nightly and recorded it as UB under both Stacked and Tree Borrows: "a `&T`
  taken before the write and used after it". `items()` is definitionally that
  row — the caller Derefs `&ArrayData` out of the `Gc`, the write happens under
  it, and `&self.items` is derived from it afterwards. `gc_contents_mut` fixes
  how the *pointer is derived*; the hazard here is the caller's live `&self`,
  which the primitive's own `# Safety` clause explicitly disclaims. Adopting it
  would move the code under a safety comment that does not cover it — strictly
  worse than the status quo, because the UB would then look audited.
- **It cannot be reached.** `sync_native_items` has `&self`; an enclosing
  `Gc<ArrayData>` cannot be recovered from a `&ArrayData`. Changing the
  signature to take the handle re-signatures every `items()` site *and* kills
  `impl Deref for ArrayData` — `Deref::deref` takes `&self` and cannot be
  handed a `Gc`. That is the same thousands-of-sites change as shape 1.

The right primitive is a **field-level `UnsafeCell`** (wrapped in a
`SyncUnsafeCell<T>` newtype carrying the `unsafe impl Sync` that
`GcBox<ArrayData>: Sync` requires), which makes the shared borrow
`SharedReadWrite` over those bytes so the write does not pop it. ADR-0013's
primitive is for *handle-holding structural writes*; this is a *read-path cache
fill*. Two primitives, one rule for choosing, both safety docs naming the other.

### Correction 2: shape 1's cost is undercounted above

The section above sizes shape 1 at "37 sites". That is only the direct
`items()` calls — but `impl Deref for ArrayData` **is** `self.items()`, so every
`&*array_data` in the interpreter reads through this chokepoint. The real figure
is thousands, which is also why an ADR-0013-style "state the obligation and
audit the call sites" resolution is not available here: 62 enumerable sites can
be audited, `Deref` cannot.

### The design, in brief

Collapse `native_storage` / `native_dirty` / `native_snapshot` into one
optional heap allocation, with the cache behind the cell:

```rust
pub struct ArrayData {
    items: Vec<Value>,                    // authoritative for an ordinary array
    native: Option<Box<NativeBacking>>,   // None for the overwhelming majority
    // ...unchanged metadata fields
}
struct NativeBacking { node: Gc<BufData>, cache: SyncUnsafeCell<DecodeCache> }
struct DecodeCache { generations: Vec<Box<Vec<Value>>>, dirty: bool, snapshot: Option<Vec<u8>> }
```

- **`items()` keeps its `&self` signature**, so all 37 `items()` and 62
  `items_mut()` call sites are untouched. The three fields are private, so
  nothing outside `src/value/` names them; external access already goes through
  `native_storage_node()` / `native_storage_address()` /
  `native_repr_body_address()`, which keep their signatures.
- **Ordinary arrays pay nothing** — the non-native arm is a plain field read
  behind a discriminant check the current code already performs.
- **The generation graveyard** is what makes it sound rather than merely
  well-typed: a re-sync pushes a *new* `Box<Vec<Value>>` instead of overwriting
  the slot, so a `&Vec<Value>` handed out earlier stays valid by construction.
  Growth is bounded by *observed C writes*, not by reads (the snapshot
  comparison short-circuits an unchanged buffer), and every `&mut self` method
  prunes it — the borrow checker there proves no shared borrow is live.

### Three more defects found while designing, all in the same twenty lines

1. **`items_mut()` never syncs before marking dirty.** If C wrote the buffer and
   Raku has not read the array since, `items` is stale; marking dirty makes the
   next sync take the *encode* branch and write that stale cache back over the
   native bytes, discarding the C write. A plain logic bug, present in debug too.
2. **The read path clones the entire native buffer on every access.**
   `let current_bytes = node.bytes.clone();` exists only to compare against the
   snapshot, so every `items()` — every element read of a native array —
   allocates and copies the whole payload. A slice comparison needs no
   allocation.
3. **`ptr::write` leaks the superseded `Vec<Value>`** (it does not drop). That
   leak is load-bearing by accident: it is the only reason an outstanding
   `&Vec<Value>` does not dangle today. The graveyard replaces it with a
   bounded, reclaimed version of the same property.

### The blast radius is wider than subtest 6 — a second repro

Written for the design pass; it is a **lost write**, not just a stale read, and
it hits a different index than the one written:

```raku
my int @a = 10, 20, 30;
my $payload = nativecast(CArray[int64], nativecast(MVMArrayB, Pointer.new(@a.WHERE)).any);
$payload[2] = 99;   # C writes index 2; Raku has not read @a since
@a[0] = 7;          # a Raku-side write to a *different* index
say "{ @a[0] } { @a[2] }";
```

`target/debug/mutsu` prints `7 99`; `target/release/mutsu` prints **`7 30`** —
the C write is gone. (Rakudo segfaults on this program: a retained `VMArray`
body pointer is only valid until the container resizes, per ADR-0015 §5.3. Raku
is not a usable oracle for this shape; the debug build is.)

### Migration and verification

Five ordered steps in ADR-0030 §4. Step 1 — the two plain logic bugs above (1
and 2) — is ~10 lines, fixes real lost writes in *both* builds, and is worth
landing on its own ahead of the representation change, since it makes the
remaining failure attributable to the UB alone.

Acceptance is release/debug **equality**, not either build's absolute result:

```sh
cargo build --release
timeout 30 target/release/mutsu t/native-array-storage.t     # 8/8, was failing subtest 6
```

### Miri: the CI gate cannot currently see this code

The Severity section above asks whether CI's `miri` job covers this function.
**The trigger does; the filter does not.** `scripts/ci-docs-only.sh --gc-value`
already classifies `src/value/**` as gc-value, so the job fires — but the step
runs `cargo miri test ... --lib gc::`, a substring filter that reaches `gc::`
and `value::value_gc::` and would **not** match a new
`value::native_cache_shapes::` module. Without widening it the job goes green
having executed none of the new probes. ADR-0030 §5 makes that widening a
required step and specifies the five shapes the probe module must pin, modeled
on `src/gc/borrow_shapes.rs`.
