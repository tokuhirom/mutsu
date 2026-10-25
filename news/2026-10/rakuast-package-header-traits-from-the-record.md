# Class, role and grammar headers record the traits as written

A class, a role and a grammar did not keep the traits of their header the way a `package` or a
routine does, so `convert` rebuilt the `traits` list from the fields the parser folds a header into:
`parents` / `does_parents` / `hidden_parents` / flags for a class, the leading `DoesDecl` statements
plus a `header_order` of positions for a role. The rebuild lost the written order (`is rw` after
`does` / `hides`), dropped the arguments of `is NAME(1, 2)` and rendered `is NAME<a b>` as a
parenthesised string.

The header parsers now note each clause as it is read (`is P`, `does R[...]`, `hides H`, `is rw`,
`is hidden`, `is export`, `is NAME(ARG)`, `is NAME<WORDS>`), in a parse that keeps spellings, and
`convert` renders the `traits` from that record in written order. `convert::class_traits`,
`decl_traits::class_custom_traits`, `role_header::ordered_traits` and `Stmt::RoleDecl::header_order`
are gone. A class trait with an argument list now receives the whole list on the ordinary frontend
too (`class C is mytrait(1, 2)`).

`t/rakuast/rakuast-package-header-traits.t` pins it and passes on rakudo 2026.09.
