# The numeric abs, sign and rounding methods are method-table rows

The next family of ADR-11276's third slice moves into the built-in method
table: `abs`, `sign`, `floor`, `ceiling`, `round` and `truncate` called with no
arguments. Each one is now a row owned by `Int`, `Num`, `Rat`, `FatRat` and
`Complex`, because Rakudo has a copy of each in every one of those method
tables. The one exception is `Complex.sign`, which Rakudo inherits from `Cool`.
Each method has one handler, and every owner's row and the remaining cascade
arms call it.

Three answers are fixed along the way:

- A `Num` past a machine word used to saturate at `i64`. `1e30.floor`,
  `.ceiling` and `.truncate` now give
  `1000000000000000019884624838656`, as in Rakudo.
- A word-sized `Rat` now rounds exactly. Before, `.round` went through an
  `f64`, so `(9007199254740993/2).round` came out one too low.
- Negating a rational whose numerator is `i64::MIN` keeps it a `Rat`. Before,
  it degraded to a `Num`, and `.abs` did the same.

Callgrind, profiling build, 200,000 calls each, second run:

| benchmark | before | after | change |
| --- | ---: | ---: | ---: |
| `$c.abs` (Complex) | 1,854M | 325M | -82.5% |
| `$r.floor` (Rat) | 1,874M | 374M | -80.1% |
| `$n.round` (Num) | 1,091M | 293M | -73.2% |
| `$i.sign` | 1,042M | 284M | -72.8% |
| `$i.abs` | 1,027M | 284M | -72.4% |
| `$n.floor` (Num) | 1,066M | 294M | -72.4% |
| `$r.round` (Rat) | 1,905M | 980M | -48.5% |
| `$i.chars` (no row) | 1,129M | 1,129M | 0.0% |
