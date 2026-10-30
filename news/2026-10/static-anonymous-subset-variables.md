# Variables with `where` are typed by an anonymous subset in the CHECK-time tree

For `static` 0.0.1 (#15610), the `$*CU` tree now has rakudo's shape for
`my Int $x where * > 0`: the declaration's `type` is an anonymous
`Type::Subset` (over the written type, carrying the `where`), whose
`compile-time-value` is a real subset type named `<anon>`; the declaration's
`where` stays reachable and its `return-type` is that subset. A named `subset`
is declared when its `Type::Subset` is asked for its `compile-time-value`, so
the analysis sees the type (and `.^refinee`) at CHECK time as in rakudo, and
`VarDeclaration::Simple` lists `return-type` / `original-type` in `.^can`.

`static` 0.0.1 against rakudo (`not ok` before -> after): t/02 5 -> 3, t/05
6 -> 5, t/08 47 -> 24, t/11 24 -> 12, t/15 60 -> 48; t/01, t/07, t/13 and t/14
match.
