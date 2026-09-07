# A container's `.WHICH` identity is a never-reused id, not its address

```raku
say [1, 2].WHICH eq [3, 4, 5].WHICH;   # raku: False   mutsu was: True
```

The two arrays differ in length *and* contents and still collided, which is
what rules out the obvious "mutsu computes a content hash" explanation.
`Array`/`Hash` `.WHICH` was already reference-based, exactly as raku's is — it
was derived from the container's **address**:

```rust
ValueView::Array(items, ..) => format!("Array|{:p}", crate::gc::Gc::as_ptr(&items)),
```

An address is only unique among *live* objects, and a `.WHICH` string always
outlives its object. mutsu evaluated the left `.WHICH`, dropped the array, then
allocated the right one — and the allocator handed back the very same block, so
the two identity strings were byte-identical. Rakudo has the same in-principle
hazard; MoarVM's allocator simply does not recycle that quickly.

## The fix

`crate::value::which_id::WhichId` is a lazily minted, monotonically increasing
id that is never reused, embedded in `ArrayData`, `HashData`, `SeqCore`,
`PromiseState` and `ChannelState`. It is not minted at construction: a container
whose identity is never asked for pays only the eight untouched bytes.

`Clone` deliberately yields a **fresh, unminted** id rather than copying,
because these types are cloned in exactly two situations and resetting is right
for both: a genuinely new container (`my @b = @a`) must not inherit `@a`'s
identity, and a `Gc::make_mut` copy-on-write rebuild produces a new allocation —
which is what the address-derived identity already reported as a new object. So
the id is never *less* stable than the pointer it replaces; it only stops two
unrelated objects from colliding. In-place mutation of a live container goes
through the aliased write path rather than `make_mut`, so `.WHICH` still
survives a `push`, as in rakudo.

`Slip`'s payload is a bare `Arc<Vec<Value>>` with nowhere to embed a field, so
it uses `slip_which_id`: a side table keyed by address like the buggy scheme,
but each entry keeps a `Weak` to the object its id was minted for. When the
allocator recycles an address the weak no longer upgrades and a fresh id is
minted — the same guarantee, reached the other way round. Dead entries are swept
whenever the table doubles.

## Cost, measured

The ticket asked for a size/allocation measurement before touching the two
hottest container types, since this is a layout change. Measured 2026-09-07:

| | before | after |
|---|---|---|
| `size_of::<ArrayData>()` | 200 | 208 |
| `size_of::<HashData>()` | 200 | 208 |
| `bench-array` allocations / bytes | 201632 / 9124348 | 201632 / **9124596** |
| `bench-hash` allocations / bytes | 314911 / 9737155 | 314911 / **9737267** |

Exactly the field, with no padding growth, no new allocations at all, and under
0.003% more bytes on the array/hash benchmarks. The sizes are pinned by a unit
test so a later field cannot slip in unmeasured.

`.WHERE` stays address-derived on purpose. `===`, `eqv` and object-hash keying
were already correct (`values_identical` compares live values with
`Gc::ptr_eq`, so it never sees a recycled address) and are unmoved.

Pinned by `t/which-identity-is-never-reused.t`, whose 14 assertions pass
unchanged under rakudo, plus the unit tests in `src/value/which_id.rs`.
