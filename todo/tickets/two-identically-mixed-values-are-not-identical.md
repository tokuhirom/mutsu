# `(1 but A) === (1 but A)` is False

Found 2026-09-06 while working
`todo/tickets/role-mixin-name-carries-a-spurious-type-parameter.md`. It
reproduces on `main` before that fix and is not caused by it — that fix removed
one contributor (the spurious type arguments in the composition key) and this
survived it.

## Repro

```raku
role A { }
say (1 but A) === (1 but A);   # raku: True   mutsu: False
```

With an attribute-carrying role and equal initialisers, too:

```raku
role R { has $.x }
say (1 but R(2)) === (1 but R(2));   # raku: True   mutsu: False
```

## Narrowed — the TYPE is identical, only the values are not

```raku
say (1 but A).WHAT =:= (1 but A).WHAT;   # both: True
```

So ADR-0060's composition key is doing its job: two separate `but A`
compositions produce the same `.WHAT`. `===` on the *values* still disagrees.

`Int` is a value type, so `1 === 1` is `True` and mixing an identical role into
each must not change that. The suspect is the per-application stamp
`__mutsu_role_seq__{name}` (`src/runtime/types/roles.rs`), which is deliberately
`next_instance_id()` — a fresh monotonic number at every composition, recorded so
that method-name collisions between two mixed-in roles resolve later-wins
(`todo/tickets/mixin-role-order-not-tracked.md`). If `values_identical` compares
the whole mixin map, that stamp makes every composition unique by construction.

`.WHAT` already gets this right by building its key from the *composition*
markers only (role name, role id, type arguments) and deliberately excluding the
bookkeeping keys — so the fix is likely to make `===` on a `Mixin` compare the
same normalized key plus the inner value, rather than the raw map.

## Where to look

`runtime::utils::values_identical`'s `Mixin` handling, and
`src/value/types.rs`'s composition-key builder (the ADR-0060 one that `.WHAT`
uses), which already draws exactly the include/exclude line this needs.

## Neighbourhood to check when fixing

Two *different* roles must stay non-identical (`(1 but A) === (1 but B)` is
False); two same-named roles from different lexical scopes must stay
non-identical (that is what `__mutsu_role_id__` exists for, and there is an
existing pin on it); different initialisers must NOT make them differ --
`(1 but R(2)) === (1 but R(3))` is **True** in raku, measured, because `===` is
`.WHICH` on the base value plus the composed type and both are `Int+{R}`
holding 1; a reference-type base
(`([1,2] but A) === ([1,2] but A)` is False in raku, because `Array` is a
reference type — the mixin must not make it True); `eqv`; and `.WHICH`.
