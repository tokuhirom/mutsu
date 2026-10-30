# `$obj[*]` and `$obj[*, ...]` on a class with its own `AT-POS`

A Whatever subscript on a user class composing `Positional` with its own `AT-POS`
answered `Nil`, because the Whatever was passed to `AT-POS(Int:D $i)`. Now a lone
`$obj[*]` is the object's `.list` (rakudo's `postcircumfix:<[ ]>` for `Whatever`),
and a Whatever among a slice's indices (`$obj[*, 1]`, `$obj[(*,)]`, a `+offsets`
slurpy forwarded as `$obj[offsets]`) reads every element as one nested list.
`annotations`' `Package(*)` (`ANN[$?CLASS][offsets]`) depends on it; with the
`+@a is raw` and Map-`eqv` fixes all of its tests now match `raku`.
