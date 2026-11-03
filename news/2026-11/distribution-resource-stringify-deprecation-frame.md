# `~$r` of a `%?RESOURCES` entry is deprecated from inside `Mu`

`Str` of a `Distribution::Resource` is deprecated in rakudo (#16055). When it is
reached by `~$r` or interpolation, rakudo's notice lists only
`SETTING::src/core.c/Mu.rakumod, line 867` under `seen at:`: the deprecated
`Str` is called from `Mu.Stringy`, never from the user's line. mutsu named the
user's line. The `Mu.Stringy` row now records the notice at that frame for a
`%?RESOURCES` entry, and a direct `.Str` keeps the user line. Pinned by
`t/modules/compunit/distribution-resource-stringify-deprecated.t`.

Not matched: a program that uses both a direct `.Str` and `~$r` lists the user
frame before the `SETTING` one in rakudo; mutsu lists them in the order they ran.
