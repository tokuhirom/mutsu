# A `Set`'s own `.WHICH` ignores its elements' user-defined `.WHICH`

Found by the 2026-09-06 doc-diff sweep (`raku-doc/doc/Language/operators.rakudoc:2376`),
re-verified against raku v2026.07 the same day.

## Repro

```raku
my class A {
    has $.a;
    method WHICH { ValueObjAt.new: "A|$!a.WHICH()" }
}
say Set(A.new(a => 5)) eqv Set(A.new(a => 5));
# raku: True   mutsu: False
```

## Narrowed — the elements agree, the Sets do not

```raku
my $s = Set(A.new(a=>5));
my $t = Set(A.new(a=>5));
say $s.keys».WHICH;      # both: (A|5)
say $t.keys».WHICH;      # both: (A|5)
say $s.WHICH eq $t.WHICH; # raku: True   mutsu: False
say $s === $t;            # raku: True   mutsu: False
say $s eqv $t;            # raku: True   mutsu: False
```

The user `WHICH` is honoured for the *element* (`A.new(a=>5).WHICH` is `A|5` in
both, and the two instances compare equal by `WHICH`) and the `Set` correctly
holds one key. What differs is the **`Set`'s own `.WHICH`**: mutsu builds it
from something other than the elements' `.WHICH` strings — object identity,
presumably — so two structurally identical Sets get different value identities.

`===` and `eqv` both read `.WHICH`, so all three rows above are one defect.

## Where to look

Set/Bag/Mix `.WHICH` construction (`src/builtins/methods_0arg/`, the QuantHash
identity path) and how set keys are encoded. Note that mutsu already has a
`.WHICH`-encoded key path for **object hashes** (`hash_uses_typed_keys` /
`value_which_key`, used by `my %h{Any}`), so the machinery to ask an element for
its `.WHICH` exists — the Set identity path is not using it.

## Neighbourhood to check when fixing

`Bag`/`Mix`/`SetHash`/`BagHash`/`MixHash` receivers; `.WHICH` on a `List`,
`Array`, `Hash` and `Pair` holding such elements; a user `WHICH` that is *not*
a `ValueObjAt` (raku requires `ValueObjAt` for value semantics — check mutsu
does not accept a plain `Str`); and `%h{$obj}` object-hash keys, which should
already be correct and must stay so.
