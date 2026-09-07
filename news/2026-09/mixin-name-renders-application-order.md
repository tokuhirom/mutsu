# A mixin's name renders one bracket per application, in order

```raku
role A { }
role B { }
say ((1 but A) but B).^name;
# raku:  Int+{A}+{B}
# mutsu: Int+{A,B}
```

Two things were wrong, not one. `role_mixin_suffix_excluding` put every
composed role inside a single `+{...}` pair, and it **sorted the names
alphabetically** — "HashMap iteration order is non-deterministic; sort for a
stable name" — so `(1 but B) but A` rendered as `Int+{A,B}` too. The property
the sort threw away is exactly the one that distinguishes the two.

## Order and grouping are part of the composed type

The same normalization reached ADR-0060's composition key, so the two orderings
resolved to one shared `.WHAT` node and `=:=` answered `True` where raku
answers `False`. That is why the fix moves the name and the type identity
together rather than just changing a separator.

mutsu already recorded the order: every composition stamps
`__mutsu_role_seq__{name}` with a monotonic id, added so later-wins method
resolution could sort by it. The name, `mixin_composition_key` (`.WHAT`) and
`mixin_identity_key` (`===`) now all order by that stamp. None of them puts the
stamp's **value** in a key — two separately-built instances of the same
composition have different stamps and must still share a `.WHAT`, still be
`===` and still be `eqv` (the punned-role identity invariant
`roast/S14-roles/instantiation.t` pins, and the reason an earlier attempt to
include the stamp itself was reverted). Only the order it encodes is kept.

## One `but` over a list is ONE application

Measuring rakudo turned up a distinction the alphabetical join had been hiding:

| written | raku |
| --- | --- |
| `(1 but A) but B` | `Int+{A}+{B}` |
| `1 but (A, B)` | `Int+{A,B}` |
| `(1 but (A, B)) but C` | `Int+{A,B}+{C}` |
| `(1 but C) but (A, B)` | `Int+{C}+{A,B}` |

and the two-role forms are *different types*: `(1 but (A, B)).WHAT =:= ((1 but
A) but B).WHAT` is `False`. So the bracket is per **application**, not per
role — which the old comma-join happened to get right for the list case, and
which `t/but-role-list.t` had been pinning.

The per-role sequence stamp cannot tell the two apart (both give A and B
consecutive stamps), so compositions now also carry
`__mutsu_role_group__{name}`: one id per `but`/`does`, shared by every role that
application composes. The compiler splits `but (A, B)` into one
`ButMixinTupleElem` per element, so that opcode gained a `first` flag; the
leading element opens a group (`open_role_application_group`) that the rest join,
and an ordinary single-role `but`/`does` closes it. Like the sequence stamp, the
group's raw id never enters a key — the key records the *partition* it induces,
ranked 0, 1, 2, …

## Pins

`t/mixin-name-application-order.t` — new, 20 assertions, **each also passing
under rakudo v2026.07**: the name for one, two and three sequential
compositions, over `Int` and `Str`, with a parameterised role; all four list
spellings; that two orderings and the list-vs-sequential pair are different
types and not `===`; and that two separately-built identical compositions still
share a `.WHAT` and are still `===` and `eqv`.

The 147 existing `t/*mixin*` / `t/*role*` files (1360 assertions) and the 186
whitelisted `roast/S12-*`, `S14-*` and `S02-types/*` files (10559 assertions)
are green.

## Residual

The anonymous role a `but <non-role>` mints carries no sequence stamp and lives
under a single key, so it is always appended last (right for `(1 but A) but
"x"`, wrong for `(1 but "x") but A`) and a second one overwrites the first.
Filed as `todo/tickets/anon-role-mixin-has-no-order-and-collapses.md`; the
marker's presence is also what distinguishes a value mixin from a genuine
allomorph, so splitting it is more than a rename.
