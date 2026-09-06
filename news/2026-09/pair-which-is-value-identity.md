# `Pair.WHICH` is the pair's content again, not a per-object counter

A `Pair`'s identity is value-based in Raku: it is composed from the key's and
the value's own `.WHICH`. mutsu's `.WHICH` *method* had no `Pair` case, so a
`Pair` fell through to the tail of the arm — a global counter that mints a fresh
number on every call:

```raku
say (a => 1).WHICH eq (a => 1).WHICH;   # raku: True   mutsu (before): False
say (a => 1).WHICH;                     # mutsu (before): Pair|2
```

That was not only wrong between two structurally identical pairs; the string was
not even stable across two reads of the *same* pair, nor across runs.

## What changed

The correct encoding already existed: `runtime::utils::value_which_key` has
proper `Pair` / `ValuePair` arms that recurse through the key's and value's own
keys, and they are what `Set`/`Bag`/`Mix` element keying has always used. The
`"WHICH"` arm of `src/builtins/methods_0arg/dispatch_core_coerce.rs` now
delegates to it rather than inventing a second encoding.

Whether a `Pair` *is* value-identified is not a property of the `Pair` but of
what it holds, and rakudo is explicit about it (commit 5031dab3ac, pinned by
roast's "Clone of Pair does not share .WHICH"): `(foo => 100).WHICH` is a
`ValueObjAt` digest, while `(foo => [1, 2]).WHICH` and `(foo => $v).WHICH` (a
container-held value) are the object's address. A value that can change under
the pair cannot be summarised by a content digest. A new `has_value_identity`
helper in the same arm states that rule once — recursing through a `Pair`'s key
and value — and decides both the encoding and the `ValueObjAt`/`ObjAt` answer.
Measured against raku v2026.07: `Int`/`Str`/`Rat`/`Set`/`Pair`/a type object are
value-identified; `Array`/`List`/`Hash`/a `Scalar`-held value are not.

## What did not move

`===`, `eqv` and object-hash keying were **already** correct, because
`runtime::utils::values_identical` compares `Pair`s structurally without going
through the `.WHICH` method. `t/pair-which-is-value-identity.t` asserts them as
the thing that must not regress, alongside the fixed rows: identical pairs
matching, the adverbial (`:a(1)`) and quoted-key spellings being the same pair,
a differing key or value being a different pair, an `Int` key not colliding with
the `Str` `"1"`, nested pairs, a stable identity across two reads, the
`ValueObjAt` type, an object hash keyed by a pair, a `Set` collapsing two
identical pairs, and the container/reference-valued pairs that must keep object
identity (including the clone case, before and after the container is written
to). 27 assertions, all measured against raku v2026.07 first.

The one thing deliberately not pinned is the exact spelling. Rakudo renders a
`Pair`'s identity as an opaque digest (`Pair|58DC55B1E331A3A1...`) which nothing
can reproduce or assert on; mutsu spells the components out
(`Pair|Str|a|Int|1`), which is stable, structurally correct, and consistent with
its other `.WHICH` strings.

## Re-diagnosed while measuring

The sibling ticket for `Array`/`Hash` `.WHICH` over-equating turned out to have
the wrong root cause on file — it is not a content hash. Both types already key
by allocation pointer, and two containers held in *variables* compare correctly;
the failure is two **temporaries**, where the first is dropped before the second
is allocated and the allocator hands back the same block. `[1,2].WHICH eq
[3,4,5].WHICH` is `True` in mutsu, which no content hash would produce. The
ticket was rewritten with that diagnosis and renamed to
`todo/tickets/array-and-hash-which-collides-on-a-reused-address.md`; the fix it
now describes is a stable per-object id on `ArrayData`/`HashData`, which is a
layout change on the two hottest container types and wants a measurement, not a
drive-by.
