# The `gc_contents_mut` borrow audit: 60 of 62 sites cleared, and the rule turns out to be about order

`gc::gc_contents_mut` is the codebase's single aliased-container-write primitive, and it takes
`&Gc<T>` while returning `&mut T`. That signature means the borrow checker offers **nothing** at its
62 call sites: keeping a `Deref`'d `&T` alive across the write compiles fine, and it is the one
failure mode Miri actually catches. `todo/deep/adr-0013-unsafecell-does-not-license-live-shared-borrows.md`
had asked for an audit of whether any site does that; it was blocked until `gc::soundness_smoke`
could run under Miri, which it now can.

## Narrowing first, reading second

A site whose `Gc` argument is never mentioned again inside its own block cannot hold a borrow across
the write and is clear by construction. That is 38 of the 62 outright. Of the rest, most turned out
to be artifacts of how the code reads: the later mention is the `&mut`'s own field where the
variable happens to share its name (`data.items` where the argument is also `items`), or it is
`Gc::make_mut` on the *other* branch of the same aliased-vs-unique `if`.

What remained was a genuine question the whole audit turned on: which operations touch the payload
and which touch only the `GcBox` header? Real call sites do all of these with the `&mut` still live —
`fixup_circular_array_refs` holds a `&mut ArrayData` across `result_arc.clone()` *and* across
passing `&result_arc` to a recursive helper.

## Measuring instead of reasoning

Reasoning says `Gc::clone` bumps `header.strong`, `as_ptr` projects through the `UnsafeCell` with
`raw_get`, and the counts live in the header, so none of them dereference the payload. But
"reasoning says" is exactly what the ADR-0013 over-promise was. `src/gc/borrow_shapes.rs` pins it
instead — probes that fail the moment one of those operations starts going through `Deref`:

| shape | verdict |
| --- | --- |
| `Gc::clone` of the same node while the `&mut` is live (self-reference build) | sound |
| `Gc::as_ptr` while the `&mut` is live (identity comparison) | sound |
| `Gc::strong_count` / `Gc::ptr_eq` while the `&mut` is live (the routing decision) | sound |
| whole-payload overwrite whose replacement carries a handle to the node being overwritten | sound |
| raw pointer derived from the `&mut`, **then** a `Deref` read, **then** a write through the pointer | sound |
| `&T` Deref'd **before** the write and **used after** it | **UB**, both models |

The last two rows are the same two operations in the two possible orders, and only one order is
sound. A raw pointer derived first sits *below* a later shared read on the borrow stack, so the read
pushes above it rather than popping it; a `&T` taken first is popped by the write. So the obligation
at a call site is not "never touch the node" — it is: **anything carried across the write must be a
raw pointer or a handle-level operation, and a `Deref`'d `&T` must not be used after it.**

The UB row is deliberately not a test. A test that triggers UB fails the gate rather than
documenting it, so it lives in the todo file's measurement table; the sound rows are what run in CI.

## Result

No site was found holding a `Deref`'d borrow across its write. Sixty of sixty-two are clear.

The remaining two are `nativecall.rs`'s `marshal_arg` (`CType::Buf`) and `marshal_carray_arg`, the
only family that lets a pointer derived from the `&mut` outlive the call — it is handed to C and the
node is retained beside it, so the derived tag must survive an arbitrary window of C-side writes. By
the table that shape is sound, and it stores only the node and the pointer, never a reference. But
the Miri job drops FFI on purpose (`--no-default-features --features native`), so the real path can
never be checked; `nativecall_shape_raw_pointer_survives_a_later_deref` is its stand-in and should
not be deleted as "not a real call site". Re-examine if that struct ever grows a borrow.

The probes run in the Miri job's first step, which keeps the leak check ON — so the two
self-referential probes sever their own edge before returning rather than leaving the collector a
cycle to reclaim.

What is still owed is the ADR-0013 §8 note. The primitive's own doc comment currently repeats the
over-promise ("valid provenance **even while shared `&` reads via `Deref` are live**") one paragraph
above a `# Safety` clause that says the opposite — and the measurements back the Safety clause. That
sentence is what a future call site would read before deciding it may keep a borrow, so the doc fix
and the ADR note belong together.
