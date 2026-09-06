# `.^roles` on a `but`-mixed value omits the mixed-in role

Found 2026-09-06 while working
`todo/tickets/role-mixin-name-carries-a-spurious-type-parameter.md` (the naming
fix, `news/2026-09/role-mixin-init-value-is-not-a-type-parameter.md`). Neither
that fix nor its diff touches `.^roles`; this reproduces on `main` before it.

## Repro

```raku
role A { }
say (1 but A).^roles.map(*.^name).join(",");
# raku:  A,Real,Numeric
# mutsu: Real,Numeric
```

The composition itself is fine — `(1 but A).^name` is `Int+{A}` in both, and
`(1 but A) ~~ A` / `.does(A)` are both `True` in both. Only the `.^roles`
listing drops it.

## What is already correct, and why that narrows it

| Program | raku | mutsu |
|---|---|---|
| `class C does A { }; C.^roles.map(*.^name)` | `A` | `A` — correct |
| `(1 but A).^name` | `Int+{A}` | `Int+{A}` — correct |
| `(1 but A).does(A)` | `True` | `True` — correct |
| `(1 but A) ~~ A` | `True` | `True` — correct |
| **`(1 but A).^roles.map(*.^name)`** | `A,Real,Numeric` | **`Real,Numeric`** |

So the class-declaration path records composed roles for `.^roles` and the
mixin path does not: `.^roles` on a `Mixin` value is answering for the BASE type
(`Int` does `Real` and `Numeric`) and never consults the `__mutsu_role__{name}`
markers the mixin carries. Note the ordering raku uses — the mixed-in role comes
first, ahead of the base type's own roles.

The parameterised spelling has the same gap, with its arguments:
`(1 but P[Int]).^roles.map(*.^name)` is `P[Int],Real,Numeric` in raku and
`Real,Numeric` in mutsu.

## Where to look

The `^roles` arm of the meta-object dispatch, and how it treats a
`ValueView::Mixin`. `src/value/types.rs`'s `role_mixin_suffix_excluding` already
enumerates exactly the markers this needs (it is what builds the `+{A}` half of
`.^name`, and `role_mixin_suffix_entry` already renders a parameterised role's
arguments), so the fix is probably to reuse that enumeration rather than to
re-derive it — prepending the mixin's roles to whatever the base type answers.

## Neighbourhood to check when fixing

Two roles mixed in successively (`(1 but A) but B` — check the order raku
reports); a role mixed into an instance of a user class that already composes
roles; `.^roles(:!transitive)` if mutsu supports the adverb; `.^mro` (which is
its own ticket, `todo/tickets/mro-includes-composed-roles.md`, and must not
start listing mixins as a side effect); and role punning
(`R.new`, where `.^name` deliberately reports plain `R` rather than `R+{R}`).
