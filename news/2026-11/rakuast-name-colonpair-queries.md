# `RakuAST::Name` gains its colonpair queries and Bool predicates

`RakuAST::Name` now has the introspection and modifier methods its new
raku-doc page documents:

- `has-colonpair($key)` and `first-colonpair($key)`;
- `without-colonpair($key)`;
- `add-colonpair($pair)`, which answers the pair as rakudo does;
- `is-package-search`, true for a name that starts with `::`.

`base-name` drops the trailing `::` of a package lookup and keeps the
colonpairs. `canonicalize` spells an indirect part as its source:
`::($x)::Foo` is `($x)::Foo`.

The predicates (`is-identifier`, `is-multi-part`, `is-global-lookup`,
`is-package-lookup`, `is-pseudo-package`, `is-installable`, `is-empty`,
`is-anonymous`, `is-indirect-lookup`) answer `True`/`False` as rakudo 2026.11
does, instead of `1`/`0` or `Mu`. `simple-identifier` of a multi-part name is
`""` (#18976).
