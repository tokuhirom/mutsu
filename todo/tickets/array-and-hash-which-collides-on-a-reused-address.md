# `Array`/`Hash` `.WHICH` over-equates two temporaries — the address is reused

Originally filed 2026-09-06 from the user-defined-`WHICH` sweep, and
**re-diagnosed 2026-09-06** while fixing the sibling `Pair.WHICH` ticket
(`news/2026-09/pair-which-is-value-identity.md`). The original diagnosis —
"mutsu computes a content hash" — is **wrong**, and following it would send you
to the wrong code. The real cause is allocator address reuse. The repro and the
divergence are real; only the explanation changed.

## Repro

```raku
say [1, 2].WHICH eq [1, 2].WHICH;      # raku: False   mutsu: True
say {a => 1}.WHICH eq {a => 1}.WHICH;  # raku: False   mutsu: True
say [1, 2].WHICH eq [3, 4, 5].WHICH;   # raku: False   mutsu: True
```

That last line is the one that rules out a content hash: the two arrays differ
in both length and contents and still collide. `(1, 2).WHICH eq (1, 2).WHICH`
belongs to the same family and answers `True` or `False` from run to run.

## Root cause — address reuse of a dead temporary

`Array`/`Hash` `.WHICH` is *already* reference-based, exactly as raku's is:

```rust
ValueView::Array(items, ..) => format!("Array|{:p}", crate::gc::Gc::as_ptr(&items)),
ValueView::Hash(map)        => format!("Hash|{:p}",  crate::gc::Gc::as_ptr(&map)),
```

(`src/builtins/methods_0arg/dispatch_core_coerce.rs`, and the same pair in
`runtime::utils::value_which_key`.) Two containers held in variables therefore
compare correctly:

```raku
my $a = [1, 2]; my $b = [1, 2];
say $a.WHICH eq $b.WHICH;   # both: False
say $a.WHICH;               # mutsu: Array|0x7f471803eee0
say $b.WHICH;               # mutsu: Array|0x7f471803f030
```

The failing case is two **temporaries**. mutsu evaluates the left `.WHICH`,
drops the array (refcount death, no live handle), then allocates the right one —
and the allocator hands back the very same block, so the two identity strings
are byte-identical. Rakudo has the same in-principle hazard; MoarVM's allocator
simply does not recycle that quickly, which is why the divergence looks like a
semantic difference and is not one.

## Why it is a ticket and not a one-liner

An address is only unique among *live* objects, so an address-derived identity
string is unsound the moment it outlives its object — and a `.WHICH` string
always does. The fix is a stable per-object id: a lazily assigned, monotonic
number stored in `ArrayData` / `HashData` (an `OnceLock<u64>` / `AtomicU64`
minted on first `.WHICH`), used in place of the pointer.

That is a layout change on the two hottest container types in the interpreter,
so it needs a size/alloc measurement before and after (`MUTSU_ALLOC_STATS=1`
with the `alloc-stats` feature, plus a bench-CI run) rather than a drive-by
edit. Weigh it against how much a correct `.WHICH` on a temporary container is
actually worth — `===`, `eqv` and object-hash keying are all **already correct**
(`runtime::utils::values_identical` uses `Gc::ptr_eq` on live values, so it never
sees a recycled address), which bounds the damage to the `.WHICH` string itself
and to code that compares those strings.

## Neighbourhood to check when fixing

`(1, 2).WHICH` (`List` takes the same `ValueView::Array` arm); `Seq`;
`.WHERE`, which is address-derived on purpose and should stay so; an empty
`Slip`, which `values_identical` deliberately treats as a singleton; and the
`Promise`/`Channel` arms right beside them, which have the identical hazard
(`format!("Promise|{:p}", ...)`) and should move together.
