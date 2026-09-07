# `(1 but A) === (1 but A)` is True

```raku
role A { }
say (1 but A) === (1 but A);   # raku: True   mutsu: False
```

`===` is `.WHICH eq .WHICH`: the base value's identity plus the composed type.
`Int` is a value type and the two compositions are the same, so raku answers
`True`. ADR-0060's composition key was already doing its job — `(1 but A).WHAT
=:= (1 but A).WHAT` was `True` — only `===` on the *values* disagreed.

## Root cause

`values_identical`'s `Mixin` arm compared the two values' raw `overrides` maps.
Every role application stamps its own `__mutsu_role_seq__{name}` — the monotonic
application-order bookkeeping that makes a later-applied role win a method
collision (`todo/tickets/mixin-role-order-not-tracked.md`, closed) — so the maps
were unique by construction and the answer could never be `True`.

The arm now compares `mixin_identity_key`, which drops exactly the two things
that are per-application or per-instance:

- **`__mutsu_role_seq__*`.** Only its absolute value is dropped; the ORDER it
  encodes is kept, because order is part of the composed type:
  `((1 but A) but C) === ((1 but C) but A)` is `False` in raku, and stays
  `False` here.
- **`__mutsu_attr__*`** (role-attribute values). `(1 but R(2)) === (1 but R(3))`
  is `True` in raku — both sides are `Int+{R}` holding 1, and `===` asks about
  the base value and the type, not the role's state.

Everything else is compared as it was, which is what keeps apart two
compositions that only the non-role part distinguishes: the allomorph `"Str"`
key (`<42> === IntStr.new(42, "forty-two")` is `False`) and `VALUE_MIXIN_MARKER`'s
fresh anonymous name per `but <non-role>` application (`(1 but "x") ===
(1 but "x")` is `False`, as in raku, where each such mixin mints its own
`<anon|N>` type).

## The inner is compared by identity, not `eqv`

The arm used `a_inner.eqv(b_inner)`, which is wrong for a reference-type base
and was only masked by the map comparison always failing. It is
`values_identical` now, so `([1, 2] but A) === ([1, 2] but A)` stays `False`
(two distinct `Array`s) while `my $a = [1,2] but A; $a === $a` is `True`.

## Scope

Pinned by `t/mixin-value-identity.t` (18 assertions measured against rakudo
2026.07; the whole file passes under `raku` unchanged): the repro over `Int` and
`Str`, `eqv`, a role carrying a method, the already-correct `.WHAT`, two
different roles, a mixed value vs its base, `but <non-role>`, both reference-type
rows, both ordering rows, both role-attribute rows, three allomorph rows, and
the same role over different base values.
