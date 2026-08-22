# Role type-pretense doesn't include `Cool`, and `.HOW.pretending_to_be` is unimplemented

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`,
`Type/Metamodel/TypePretense.rakudoc:15,47`).

## Repro 1 — `Role ~~ Cool` should be `True`

```raku
class Class { }
role  Role  { }

say Role ~~ Mu;   # True
say Role ~~ Any;  # True
say Role ~~ Cool; # True
```

- `raku`: `True`, `True`, `True`.
- `mutsu` (`target/debug/mutsu`): `True`, `True`, **`False`**.

Verified directly with `raku -e` / `target/debug/mutsu -e` on this exact snippet. (The
rest of the doc's original example — `Role.^pun.^parents(:all)` — is separately bucketed
`raku-drift`: the doc's stated `# OUTPUT: «()»` no longer matches current raku's `(Any
Mu)`, and mutsu's `(Any Mu)` there already matches current raku, so that part is not a
bug.)

## Repro 2 — `.HOW.pretending_to_be` missing

```raku
role Role { }
say Role.HOW.pretending_to_be.map(*.^name); # raku: (Cool Any Mu)
```

- `raku`: `(Cool Any Mu)`
- `mutsu`: dies with
  `No such method 'pretending_to_be' for invocant of type 'Perl6::Metamodel::ParametricRoleGroupHOW'`

## Root cause hypothesis

Per `raku-doc/doc/Type/Metamodel/TypePretense.rakudoc`, an un-composed `role` "pretends"
to be part of the `Cool`/`Any`/`Mu` chain (via `Metamodel::TypePretense`, mixed into
`ParametricRoleGroupHOW`) so that smart-matching/type-checking against those ancestor
types succeeds even before the role is composed into a class. mutsu's role type-object
smart-match (`~~`) handling implements the `Mu`/`Any` pretense levels but not `Cool`,
and doesn't implement the `.pretending_to_be` introspection method that lists the
pretended-to types at all.

## Affected files (starting point)

- Wherever role type-objects' `~~ Mu`/`~~ Any` pretense is implemented (grep for
  `"TypePretense"` or the `Mu`/`Any` role-smartmatch special-case in `src/runtime/`) —
  extend to `Cool`, and add `.pretending_to_be` returning the pretended type chain
  (`(Cool Any Mu)`) on `Perl6::Metamodel::ParametricRoleGroupHOW` (and presumably
  `ParametricRoleHOW`).
