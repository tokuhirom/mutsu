# `List.Array` decays Nil to Any; `.base` of a big Rat/FatRat

Two gaps found with the `ClickHouse` distribution (all seven test files now print what `raku`
prints).

- `.Array` of a `List` or `Seq` makes a fresh Array whose slots default to `Any`, so a `Nil`
  element decays to `Any` (`(Nil, 3).Array` is `[Any, 3]`). `.list` still keeps the `Nil`.
- `.base($radix, $digits)` of a `Rat`/`FatRat` whose numerator or denominator no longer fits a
  machine word (`'0.33333333333333333333'.FatRat`) answered "No such method 'base'": the rows
  only read word-sized components. They now use 128-bit arithmetic while both stay below 2**100.
