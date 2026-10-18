`role R hides Parent` and `role R is Parent is hidden` now round-trip through
RakuAST. Conversion emits `Trait::Hides` and the named `Trait::Is` form measured
in Rakudo 2026.09; lowering restores the role parser's existing parent and
hidden markers. The focused regression covers both node shapes, EVAL, and
unhidden MRO behavior. The RakuAST frontend ratchet now passes 7,847 of 7,904
`t/` files.
