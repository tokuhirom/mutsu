# `.^roles(:!transitive)` on a built-in type still lists the transitive roles

Measured 2026-09-07 while fixing
`todo/tickets/mixin-roles-introspection-omits-the-mixed-in-role.md`
(`news/2026-09/mixin-roles-introspection.md`). Independent of it — it
reproduces with no mixin involved.

## Repro

```raku
say 1.^roles(:!transitive).map(*.^name).join(",");
# raku:  Real
# mutsu: Real,Numeric
```

`Int` does `Real`, and `Real` does `Numeric`; only `Real` is a DIRECT role, so
`:!transitive` must stop there.

## What is already correct, and why that narrows it

The user-declared side honours the adverb:

| Program | raku | mutsu |
|---|---|---|
| `role X {}; role Y does X {}; class K does Y {}; K.^roles(:!transitive)` | `Y` | `Y` — correct |
| `K.^roles` | `Y,X` | `Y,X` — correct |
| `1.^roles` | `Real,Numeric` | `Real,Numeric` — correct |
| **`1.^roles(:!transitive)`** | `Real` | **`Real,Numeric`** |

So `collect_roles_for_class` applies `non_transitive` correctly when it walks
the registry's `role_parents`, and not at all when it answers for a built-in
type — whose role list is a flat, hardcoded set with no direct/inherited
distinction to filter on.

## Where to look

`Interpreter::collect_roles_for_class`
(`src/runtime/methods_classhow_parents.rs`) and whatever table it reads a
built-in type's roles from. The fix needs that table to record the DIRECT roles
per type (with the transitive closure derived, as the user side already does),
rather than the flattened list it stores today.

## Neighbourhood to check when fixing

`Str`/`Num`/`Rat`/`Array`/`Hash` and the other built-ins with more than one role
in their closure; `:local` (which mutsu currently answers the same as the
default for a built-in — check raku); a user class inheriting from a built-in;
and `.^roles` with no adverb, which must keep returning the full closure in the
same order.
