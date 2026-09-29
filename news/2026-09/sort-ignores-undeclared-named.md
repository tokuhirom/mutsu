# `.sort` ignores undeclared named arguments

`(3,1,2).sort(:qqzz9)` returned `(3 1 2)` because the named `Pair` was taken as a
comparator. `sort_value_generic` now drops named-flavour arguments other than `:k`/`:by`,
so Rakudo's implicit `*%_` behaviour holds and the answer is
`(1 2 3)`. Part of the residue tracked in #9905.
