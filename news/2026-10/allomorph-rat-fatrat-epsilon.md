# Allomorph invocants answer `.Rat(eps)` and `.FatRat(eps)`

`<7>.Rat("0.01")` and `<7>.FatRat("0.01")` answered 0 because the one-argument
`Rat`/`FatRat` arms did not look through an `IntStr`/`RatStr`/`NumStr` mixin.
They now treat the allomorph as its numeric inner value, so the epsilon binds
exactly where it does for `Int`/`Rat`/`Num` (#12097).
