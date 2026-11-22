# `Nil`'s `Cool` coercions bind or refuse their arguments

`Nil` is `Cool`, so `Nil.round(0.1)` is `Cool.round($scale)`, which is
`self.Numeric.round($scale)`: a "Use of Nil in numeric context" warning, then
`0` rounded to the scale (`0.0`). mutsu answered only the zero-argument forms of
these coercions, so any call with an argument was absorbed into `Nil` (#18591).

- `Nil.round($scale)` now warns and rounds `0`.
- A call no `Cool` candidate binds fails as Rakudo's does:
  - `Nil.Num(1)`, `Nil.abs(1)`, `Nil.floor(1)`, `Nil.sign(1)` and `Nil.ords(2)`
    raise "Too many positionals passed";
  - `Nil.Real(1)`, `Nil.Stringy(1)` and `Nil.round(1, 2)` raise
    `X::Multi::NoMatch`.
