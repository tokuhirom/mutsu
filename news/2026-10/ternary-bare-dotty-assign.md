# A bare `.=meth` is accepted in a ternary branch

`1 ?? .=uc !! 2` died with "Precedence of = is too loose to use inside ?? !!". The bare topic
`.=meth` lowers to the plain `$_ = $_.meth` an `andthen` chain retargets, so the ternary's
precedence check saw an ordinary assignment. The lowering now marks its topic call, and
`is_dotty_assign` recognizes it; a written `$_ = $_.uc` still needs parentheses.
