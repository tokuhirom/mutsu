# The Rational methods are rows in the built-in method table

ADR-11276's second slice moves its first whole family into the built-in method
table. `numerator`, `denominator`, `nude`, `norm` and `isNaN` are now rows owned
by `Rat` and by `FatRat`, because Rakudo composes the `Rational` role into each
of them. Both owners' rows use the same handler for each method, and that
handler also covers rationals whose components do not fit a machine word.
`Int` and `Complex` have `isNaN` rows too. The arms that used to answer these
methods in the zero-argument cascade are deleted, and `native_method_0arg`
consults the table before anything else, so every caller of the cascade gets
the row's answer.

The receivers the table can recognize grow by three: `Int` (inline, boxed and
big), `FatRat` and `Complex`. A big rational takes the shape of the type its
FatRat flag names.

Two answers change to match Rakudo. `5.numerator`, `5.denominator`, `5.nude`
and `5.norm` are now "No such method", since Rakudo's `Int` does not do
`Rational`. `.norm` now keeps the receiver's type for big components. Before,
a big `FatRat` came back as a `Rat`, and a big `Rat` whose reduced parts fit a
word came back as a `FatRat`.

Callgrind, profiling build, 200,000 calls each, second run:

| benchmark | before | after | change |
| --- | ---: | ---: | ---: |
| `$f.numerator` (FatRat) | 1,785M | 313M | -82.5% |
| `$i.isNaN` | 1,110M | 272M | -75.4% |
| `$c.isNaN` (Complex) | 1,935M | 320M | -83.5% |
| `$r.nude` | 2,228M | 596M | -73.3% |
| `$r.numerator` (already a lane hit) | 306M | 313M | +2.3% |
| `$i.abs` (no row) | 1,017M | 1,019M | +0.2% |
| `$s.uc` (no row) | 2,454M | 2,416M | -1.5% |
| `@a.map(*+1).elems` (no row) | 1,415M | 1,418M | +0.2% |
