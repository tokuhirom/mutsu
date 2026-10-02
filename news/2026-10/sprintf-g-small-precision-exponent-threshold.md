# sprintf `%g` leaves fixed notation earlier for precision 1 and 2

Rakudo switches `%g` to exponent form when the decimal exponent is below
`-min(precision + 1, 4)`, not C's fixed `-4`, so `sprintf('%.2g', 0.00012)` is
`1.2e-04`. mutsu's `format_g` now follows that rule. Found through the FStrings
distribution, whose `t/01-basic.rakutest` now passes 27/27.
