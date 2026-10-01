# `sum(...)` routine form no longer truncates Rats

`sum(1, 0.5)` returned `1` because the routine form accumulated into an `i64`/`f64` pair and cast
every other numeric type to `i64`. Operands that are not plain Int/Num (Rat, FatRat, BigInt,
Complex, Str) now fold through `+` exactly like `.sum`. Found via the `Timer` distribution, whose
`t/01-basic.t` now passes.
