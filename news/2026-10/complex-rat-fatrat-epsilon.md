# `Complex.Rat(epsilon)` and `.FatRat(epsilon)` answer the real part's value

`(3.14159+0i).Rat(0.01)` and `(3.14159+0i).FatRat(0.01)` answered `0` for every `Complex`; rakudo
answers `22/7`, as `3.14159e0.Rat(0.01)` does. The one-argument `Rat` / `FatRat` arms had no
`Complex` case and fell into their `0` default. They now decline a `Complex`, and
`Interpreter::dispatch_complex_to_real` (which already answered the zero-argument forms) takes the
epsilon too: a negligible imaginary part under `$*TOLERANCE` leaves the real part's own
`.Rat(epsilon)` / `.FatRat(epsilon)`, and a non-negligible one throws `X::Numeric::Real` instead of
answering `0`. (Rakudo dies there as well, with an accidental "Too many positionals passed"
`X::AdHoc`; mutsu keeps the exception the zero-argument forms throw.)

Along the way `Num.FatRat(epsilon)` stopped ignoring its epsilon: `3.14159e0.FatRat(0.01)` was
`3141590/1000000` and is `22/7`, sharing the `Num` to `Rat` conversion with `.Rat(epsilon)`.
