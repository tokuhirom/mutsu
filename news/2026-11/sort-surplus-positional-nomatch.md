# `.sort` with a surplus or non-Callable positional raises `X::Multi::NoMatch`

`(1, 2).sort(1, 2)` and `(3, 1).sort(1)` used to sort silently (the extra positional was taken as a
comparator, or ignored). No `sort` candidate binds two positionals or a non-`Callable` one, so they now
raise `X::Multi::NoMatch` listing rakudo's candidates from the generated table, the same way
`.sort(*)` already did -- whose hand-written candidate list is gone. A `Seq` receiver is named `List`
(rakudo's `Seq.sort` redispatches to `self.list.sort`), a `Hash` lists the `Map` candidate, a `Range`
or scalar only `Any`'s. A `Bool` named argument now reads as its colonpair (`:k`, `:!k`) in every
NoMatch call profile, as rakudo prints it. Closes #17830.
