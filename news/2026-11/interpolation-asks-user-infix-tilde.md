# A string interpolation asks the user `infix:<~>` candidates first

`"A: $a"` stringified every piece (`.Str`) before concatenating, so a user
`multi infix:<~>(Str() $a, A $b)` was never asked, and an `A.Str` that answers `self`
died with "cannot unbox to a native string" (#15932, rakudo/rakudo#6860, new in
`roast/S06-operator-overloading/infix.t`). In the scope of a user `infix:<~>`, an
interpolation of two or more pieces now folds them left to right through the candidates
with the pieces as they are, and stringifies a pair no candidate takes the ordinary way.
An interpolation inside a candidate's own body stringifies plainly instead of re-entering
it. Without a user `infix:<~>` in scope nothing changes (one scope check per interpolation).
Pinned by `t/lang/quoting/interpolation-user-infix-tilde.t`; the roast file is back in
the whitelist.
