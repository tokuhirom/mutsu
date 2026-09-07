# A class's own composed roles are reported last-declared-first

`.^mro(:roles)` listed the roles a class composes itself in **declaration**
order; raku reverses them.

```raku
role R2 { }
role R3 { }
class K2 does R2 does R3 { }
say K2.^mro(:roles).map({ .^name }).join(",");
# raku:  K2,R3,R2,Any,Mu
# mutsu: K2,R2,R3,Any,Mu   (before)
```

It is the last row of the measurement matrix from
`news/2026-09/mro-excludes-composed-roles.md` that still diverged. Only the
ordering of roles composed by the *same* class was wrong: one composed role
agreed, and so did a role composed into another role (`role RB does RA; class
KN does RB` is `KN,RB,RA,Any,Mu` in both). raku lists a class's own
compositions **last-declared-first** — the same last-wins rule
`mixin_roles_applied_last_first` already implements for a `but`-mixed role —
and `.^roles` and `.^roles(:!transitive)` follow the same order.

## The fix

The ticket pointed at `Interpreter::classhow_mro_with_roles`, but the order it
splices in is only one of four readers of the same registry rows, so fixing it
there would have left `.^roles` and `.^roles(:!transitive)` disagreeing with it.
The order is now flipped once, at the single place both composed-role lists are
written — `Interpreter::record_class_composed_roles` — so every reader agrees
by construction.

`role_lists_last_declared_first` reverses `class_direct_composed_roles`
outright, and reorders the flattened `class_composed_roles` closure by
**segment**: that list is built role by role, so it reads `[d1, tail(d1)...,
d2, tail(d2)...]` with the direct entries as its markers. Reversing the
segments — rather than reversing the whole list, or re-deriving reachability
from `role_parents` — is exactly what keeps a role reached *through* another
one after it while flipping the class's own `does` order. Anything ahead of the
first marker keeps its place.

Recording the order rather than computing it at report time is also what keeps
the built-in seeds right: `runtime_init` writes `Array`'s
`(Positional, Iterable)` and `Seq`'s `(Sequence, PositionalBindFailover,
Iterable)` straight into the registry already in rakudo's reported order, and
those rows never pass through this function. A report-time reversal had no way
to tell them from a user declaration and flipped them too.

## Composition order is a second, separate order

Rakudo keeps *two* lists for exactly this reason: `add_role` **unshifts** onto
the `@!roles` that reporting and typechecking read, while composition walks a
separately pushed `@!roles_to_compose`. mutsu has one row for both, so flipping
it surfaced the one consumer that genuinely needs the un-flipped order —
`ordered_role_submethods_for_class`, the `BUILD` / `TWEAK` / `DESTROY` walk.
`t/role-submethods-6e.t` pins it: for `class C1 does R1 does R2`, `R1.BUILD`
runs before `R2.BUILD` even though `.^roles` is `(R2, R1)`, and rakudo agrees.

Because the segment reordering is **its own inverse**, that walk recovers
composition order by applying the same function again to the recorded list.
No second registry row was needed.

The composition-conflict diagnostic, by contrast, names the contributors in the
reported order and already matched rakudo before and after: `Method 'm' must be
resolved by class KC because it exists in multiple roles (C2, C1)`.

## Measured against `raku`, all matching

`.^mro(:roles)`, `.^mro`, `.^roles` and `.^roles(:!transitive)` for one, two
and three directly composed roles, for a role composed into a role, and for
`Array`, `Hash`, `Seq`, `Blob`, `Buf`, `Set`, `Bag`, `Mix`, `Rat`, `Int`,
`List`, `Slip`, `Range`, `Pair` and `Map` — byte-identical output.

## Testing

New `t/mro-roles-last-declared-first.t` (12 assertions), which passes unchanged
under rakudo. `t/mro-excludes-composed-roles.t`, `t/roles-non-transitive-adverb.t`
and `t/role-submethods-6e.t` are untouched and still pass.
