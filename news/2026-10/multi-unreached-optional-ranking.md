# Multi dispatch ranks unreached optional parameters by their declared type

An optional positional parameter that a call supplies no argument for used to
take no part in multi-candidate ranking at all, so the winner fell to
declaration order: `multi g(Int $a?)` / `multi g(Str $a?)` ran the first for
`g()` instead of raising `X::Multi::Ambiguous`, and `multi g($a?)` beat a later
`multi g(Int $a?)`.

Rakudo builds its candidate tiers from the declared types, so such a parameter
still orders the pair. `RankProfile` now records the declared nominal type of
each unreached optional slot, and `MultiRank` compares those slots
parameter-wise: a type that conforms to the other's is narrower, the same type
wins by a refinement, and unrelated types tie. A pair split between a reached
and an unreached parameter is incomparable and, with no bind-time check
involved, ambiguous. The ambiguity check that compares dispatch shapes now
leaves unreached optionals out, and declaring optional positionals became a
yes/no narrowness step instead of a count (`multi m($x?)` / `multi m($x?, $y?)`
is ambiguous for `m()`, as in rakudo). The type relation the ranker uses is
now directional (`nominal_type_conforms`).

Fixes #12852.
