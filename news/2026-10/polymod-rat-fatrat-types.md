# polymod keeps Rat and FatRat element types

`Rat.polymod` now returns a non-zero whole element as a `Rat` (`5.Rat.polymod(.3, .2)` ends in
`80.0`, not `80`), and `FatRat` declares `polymod`, so `FatRat.new(1, 3).polymod(3)` is
`(FatRat.new(1, 3), 0)` instead of `(0, 0)`. Zero elements stay `Int`, and an `Int` invocant is
unchanged, matching Rakudo.
