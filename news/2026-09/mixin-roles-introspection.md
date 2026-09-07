# `.^roles` on a `but`-mixed value lists the mixed-in roles

```raku
role A { }
say (1 but A).^roles.map(*.^name).join(",");
# raku:  A,Real,Numeric
# mutsu: Real,Numeric
```

The composition itself was fine — `(1 but A).^name` was already `Int+{A}`, and
`.does(A)` / `~~ A` both answered `True`. Only the `.^roles` listing dropped it:
`dispatch_classhow_roles` resolved the invocant to a *class name* and answered
for that base type alone, never consulting the `__mutsu_role__{name}` markers
the `Mixin` carries.

## Order, and no dedupe

raku puts the mixed-in roles **first**, most-recently-applied first, ahead of
whatever the base type composes:

```raku
((1 but A) but B).^roles     # (B, A, Real, Numeric)
((1 but B) but A).^roles     # (A, B, Real, Numeric)
(C.new but B).^roles         # (B, A)   -- class C does A
```

The application order comes from the same `__mutsu_role_seq__{name}` stamp
`receiver_class::mixin_chain` already sorts the dispatch order by. And raku does
**not** dedupe — `(C.new but A).^roles` where `class C does A` is `(A, A)` — so
neither does this.

`mixin_roles_applied_last_first` (`src/value/types.rs`) renders each entry
through the existing `role_mixin_suffix_entry`, so a parameterised role keeps
its arguments (`(1 but P[Int]).^roles` is `(P[Int], Real, Numeric)`) and the
anonymous role a `but <non-role>` composes is listed as raku lists it
(`(1 but "x").^roles` is `(<anon|1>, Real, Numeric)`). Role punning still
reports itself once: `Pun.new.^roles` is `(Pun)`, matching the exclusion
`.^name` already applies there.

## Scope

Pinned by `t/mixin-roles-introspection.t` (14 assertions measured against rakudo
2026.07; the whole file passes under `raku` unchanged): the repro over `Int` and
`Str`, a parameterised role, both application orders, a single `but` of two
roles, a mixin over a class instance that already composes a role, the
non-deduped case, the anonymous role, and five rows that must not move (a plain
value, a class type object, a plain instance, a role with no parents, and role
punning).

Found next door and filed rather than folded in:
`todo/tickets/roles-not-transitive-is-ignored-for-builtin-types.md` —
`1.^roles(:!transitive)` answers `Real,Numeric` where raku answers `Real`. The
adverb is honoured for user-declared roles and ignored for a built-in type,
whose role list is a flat set with no direct/inherited distinction to filter on.
It reproduces with no mixin involved.
