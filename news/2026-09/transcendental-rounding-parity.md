# Match Rakudo's log10 and atanh rounding

The routine and method forms of `log10` and `atanh` now share the same formulas
as Rakudo. This fixes last-bit differences in those results and in expressions
such as `tanh(atanh(0.5))`.
