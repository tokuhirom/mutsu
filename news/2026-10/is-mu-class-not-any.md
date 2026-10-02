# An instance of a class that `is Mu` is no longer an `Any`

`class F is Mu {}; F.new ~~ Any` was True, and an untyped `$` parameter accepted
`F.new`, because the instance branch of `type_matches_value` asked the name-only
`type_matches("Any", "F")`, which answers True for every name. For a user class
the check now consults the MRO, which has no `Any` for a class declared `is Mu`.
Rakudo agrees: `F.new ~~ Any` is False and `f(F.new)` fails the implicit-`Any`
binding check. Pinned in `t/types/mu-class-not-any.t`.
