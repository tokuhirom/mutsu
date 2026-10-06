# A Complex coerces to a Real type by `$*TOLERANCE`, in one place

`(3.7+1e-20i).Int` died with "imaginary part not zero"; Rakudo answers `3`,
because `Complex.Real` accepts a number whose imaginary part is `≅ 0` under
`$*TOLERANCE` (#11795).

Six coercions asked that question six ways: `.Int` and `.UInt` accepted only an
exactly-zero imaginary part, `.Real`, `.Rat` and `.FatRat` hardcoded `1e-15`
(non-strict, never reading the variable), and only `.Num` read `$*TOLERANCE`.
They now share `Interpreter::dispatch_complex_to_real`: the builtin cascade
declines a `Complex` receiver for all six, and the handler tests the imaginary
part with the same `approx_eq_f64` that `=~=` uses (strictly below the
tolerance, absolute against zero), then hands the real part to `Num`'s own
method. So `$*TOLERANCE = 0` rejects even `3+0i`, `1+1e-15i` is rejected at the
default, and a looser tolerance accepts a larger imaginary part, as in Rakudo.

Along the way `X::Numeric::Real`'s message renders the number as `.Str` does
(`3.7+1e-20i`, not `3.7+0.00000000000000000001i`), and `.UInt` names `Int` as
the attempted type, as Rakudo does.
