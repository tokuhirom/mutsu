# Named `@` parameters see a Seq as a List in multi `where`; `round(Num, Rat)` is a Rat

Found through Math::SparseMatrix (`t/26-dot-product.rakutest`). A multi candidate such as
`multi method new(:@dense-matrix! where @dense-matrix ~~ List:D)` was rejected when the caller passed a
`Seq` (`dense-matrix => @vec.map(...)`), because dispatch evaluated the `where` clause against the raw
Seq while the binder hands the callee its cached List view. The candidate now sees the same List.

Fixing that exposed a second gap: `round(0.34567e0, 0.001)` returned a Num, but Rakudo computes
`(x / $scale + 1/2).floor * $scale` as `Int * Rat`, an exact Rat. Sums of rounded values therefore
carried float noise (`70.35900000000001`). `exact_round_scaled` now handles a finite Num target.
