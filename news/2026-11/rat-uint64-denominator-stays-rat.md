# A Rat with a uint64 denominator stays a Rat

`(1/2)**63`, `.5**63` and `(1/3)**40` degraded to `Num`: `Rat ** Int` demoted the result as soon as its
denominator left `i64`, although rakudo's `Rat` holds a `uint64` denominator and only degrades a larger
one (`(1/2)**64` is a `Num`). `make_big_rat_arith` already applied the `uint64` rule; the power arms now
keep what it returns. A big-denominator Rat that terminates in decimal also prints as its decimal in
`.raku` (`(1/2**63).raku`), not as `<1/9223372036854775808>`. Rat::Precise's `t/01-rat.t` and
`t/02-fatrat.t` now match rakudo.
