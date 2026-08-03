# ADR-0013's `UnsafeCell` claim is stronger than what Miri supports

Measured 2026-08-03 while migrating the `Mixin` overrides map to a `Gc` node
(`news/2026-08/mixin-overrides-map-is-a-gc-node.md`). Not a bug in shipped behaviour — a
load-bearing claim in an accepted ADR that the tooling does not back.

## The claim

[ADR-0013](../../docs/adr/0013-container-interior-mutability-cellvalue.md) §1.3-1 frames the old
`&mut *(Gc::as_ptr(gc) as *mut T)` primitive as "**Provenance UB (broad, every run,
Miri-detectable)** … present at all 51 sites on every execution. It is the dominant problem", and
§2 says the `UnsafeCell` route yields a `&mut` with valid provenance "**even while shared `&`
borrows into the same node exist** — the one thing `Gc::as_ptr(gc) as *mut T` cannot give". §8
then records the fix as landed, which is why the `Mixin` variant looked like the last outstanding
UB site.

## What Miri actually reports

Four probes on the gate's pinned toolchain (`nightly-2026-08-01`), each a `HashMap<String, Value>`
behind the respective smart pointer, run under the default Stacked Borrows and under
`-Zmiri-tree-borrows`:

| probe | Stacked | Tree |
| --- | --- | --- |
| `Arc::as_ptr as *mut` write, no `&T` live across it | ok | ok |
| `gc_contents_mut` write, no `&T` live across it | ok | ok |
| `Arc::as_ptr as *mut` write, a Deref'd `&T` used after the write | UB | — |
| `gc_contents_mut` write, a Deref'd `&T` used after the write | UB | — |

Two conclusions, both contrary to the ADR:

1. **The bare `Arc` shape is not flagged.** `Arc::as_ptr` reads the `NonNull` out of the handle;
   the pointer value carries the original allocation's provenance, never having passed through a
   `&T`. So the write is permitted. "Provenance UB, every run, Miri-detectable" does not describe
   this shape.
2. **The `UnsafeCell` does not license live shared borrows.** `Gc`'s `Deref` hands out a real
   `&T`; a write through `gc_contents_mut` invalidates it exactly as it would for an `Arc`. The
   `UnsafeCell` matters for how `Gc::as_ptr` *derives* the pointer — `UnsafeCell::raw_get`, with
   no intermediate reference — not for what callers may hold across the write.

The primitive's own SAFETY doc is already the accurate version ("no other `&`/`&mut` into this
value is *dereferenced* for the lifetime of the returned borrow"). It is the ADR's prose that
over-promises.

## Why this is not a one-line doc fix

The claim is what sized the campaign. §1.3 splits the debt into "broad provenance UB (dominant,
cheap to fix)" and "narrow cross-thread race (deferred to ADR-0001 layer 3c)", and §1.4 justifies
the whole ADR as "the *only* path that removes the UB". If (1) holds, the dominant problem was
mis-stated and the residual risk at the ~62 `gc_contents_mut` call sites is entirely the
*aliasing-discipline* obligation — the thing nothing mechanically checks — rather than a
representation defect that the `UnsafeCell` retired. That changes what the Miri gate is for
(catching call sites that hold a borrow across a write, which is a real and reachable mistake)
and it changes what "ADR-0013 is closed" means.

Wanted:

- Re-run the probes on a couple of nightlies to make sure this is not a single-toolchain artifact,
  and against a shape that goes through `&T` explicitly (some call sites may genuinely construct
  the pointer from a reference, which *would* be the UB the ADR describes — the inventory in
  `docs/gc-contents-mut-inventory.md` is the place to look).
- Audit whether any of the ~62 call sites holds a Deref'd borrow across its write. That is the
  failure mode Miri *does* catch. **Unblocked 2026-08-03** (#5775): `gc::soundness_smoke` now runs
  under Miri, so the vehicle exists. Progress below.
- Then amend ADR-0013 (a superseding note in §8, not a rewrite of §1.3 — the decision itself still
  stands; the `UnsafeCell` is the right representation regardless).

## Audit progress (started 2026-08-03)

**The primitive gives the borrow checker nothing to work with.** `gc_contents_mut(gc: &Gc<T>) ->
&mut T` takes a *shared* reference, so nothing stops a call site from holding a Deref'd `&T` from
the very same handle across the write. The obligation is 100% on the author at all 62 sites.

**Narrowing.** `tmp/audit_gc_contents_mut.py` (throwaway; re-create from this description) takes the
identifier passed to each call and lists every later mention of it inside the enclosing function. A
site whose argument is never mentioned again cannot hold a borrow across the write and is clear by
construction. Result: **62 sites — 20 clear, 42 to read by hand** (the 42 include false positives
from same-named bindings in sibling match arms of very long functions, which the coarse function
boundary cannot separate).

**Finding 1 — the primitive's own doc repeats the over-promise.** `src/gc/gc_ptr.rs` (the
`gc_contents_mut` doc comment, just above its `# Safety`) says the `&mut` has valid provenance
"**even while shared `&` reads (via `Gc`'s `Deref`) are live**". The `# Safety` clause immediately
below says the opposite and is the accurate one ("no other `&`/`&mut` into this value is
*dereferenced* for the lifetime of the returned borrow"), and the probes above back the Safety
clause. This sentence is load-bearing in the wrong direction: it is what a future call site would
read before deciding it may keep a borrow. Fix it with the ADR §8 note.

**Finding 2 — the rule is about ORDER, and it is now pinned.** `src/gc/borrow_shapes.rs` measures
the shapes call sites actually perform while the aliased `&mut` is live, on the gate's pinned
toolchain:

| shape | Stacked | Tree |
| --- | --- | --- |
| `Gc::clone` of the same node while the `&mut` is live (self-reference build) | ok | ok |
| `Gc::as_ptr` while the `&mut` is live (identity comparison) | ok | ok |
| `Gc::strong_count` / `Gc::ptr_eq` while the `&mut` is live (the aliased-vs-unique routing) | ok | ok |
| whole-payload overwrite whose replacement carries a handle to the node being overwritten | ok | ok |
| raw pointer derived from the `&mut`, **then** a `Deref` read, **then** a write through the pointer | ok | ok |
| `&T` Deref'd **before** the write and **used after** it | **UB** | **UB** |

So the obligation is not "never touch the node" — it is specifically: **anything carried across the
write must be a raw pointer or a handle-level operation; a `Deref`'d `&T` must not be used after
it.** `Gc::clone`, `as_ptr` and the counts all touch only the `GcBox` header (or project through
the `UnsafeCell` with `raw_get`), so they never conflict. The last two rows are the same two
operations in the two possible orders, and only one order is sound — a raw pointer derived first
sits *below* a later shared read on the borrow stack, so the read pushes above it rather than
popping it, whereas a `&T` taken first is popped by the write.

The first four rows are what clears the audit mechanically; the probes fail the moment one of those
operations starts going through `Deref`.

**Audit result: 60 of 62 sites clear, 2 rest on the FFI argument.** After the narrowing and the
probes, every site falls into one of: the argument is never mentioned again (38); the later mention
is the `&mut`'s own field, which happens to share the variable's name (`data.items` where the
argument is also called `items` — 6); the later mention is `Gc::make_mut` on the *other* branch of
the same aliased-vs-unique `if` (10); or the later mention is a `Gc::clone` / `as_ptr` cleared by
the probes above (6). No site was found holding a Deref'd borrow across its write.

The two remaining are `src/runtime/nativecall.rs` (`marshal_arg`'s `CType::Buf` arm and
`marshal_carray_arg`), the only family that lets a pointer derived from the `&mut` outlive the call:

```rust
let data_ptr = unsafe { crate::value::gc_contents_mut(&node) }.bytes.as_mut_ptr();
(Type::pointer(), ArgOwner::BufBytes { node: Some(node), buf: Vec::new(), data_ptr: ... })
```

By the table above this shape is sound — the pointer is derived first and reads through the retained
node push above it. But the Miri job runs `--no-default-features --features native` precisely to
drop FFI, so **the real path can never be checked**; `nativecall_shape_raw_pointer_survives_a_later_deref`
in `borrow_shapes.rs` is the stand-in. What would break it is an intervening `&T` taken before a
C-side write and used after it, which this code does not do (it stores only `node` and `data_ptr`,
never a reference). Re-examine if that struct ever grows a borrow.
