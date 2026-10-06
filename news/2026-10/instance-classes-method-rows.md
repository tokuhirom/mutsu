# Instance classes: 145 more built-in methods are table rows, with shapes for Instant, Duration and Match

Slice 3D of [ADR-11276](../../docs/adr/11276-built-in-methods-are-handler-rows.md) moved the methods
of `Date`, `DateTime`, `Instant`, `Duration` and `Match` into the built-in method table: 821 rows
are registered now, up from 676. `Date` and `DateTime` share one handler per `Dateish` method
(`day-of-week`, `week`, `daycount`, `yyyy-mm-dd` with its separator, ...) and add their own
(`succ`, `utc`, `hh-mm-ss`, `julian-date`, `Instant`, ...). `Instant` and `Duration` are new shapes:
both do `Real`, so a row answers the question of the seconds they hold and wraps the answer only
where the method keeps the type (`abs`, `succ`, `pred`), and every cascade arm that matched them by
class name is gone. `Match` has a shape for a plain regex match, lazy or eager, with 17 rows; a
lazy match is never materialized by a call that only reads its offsets or its text.

`Date`, `DateTime`, `Instant` and `Duration` are open to their ancestors' rows, after an audit of
every registered `Any`, `Cool` and `Mu` row against each of them: `Instant.sqrt`, `exp`, `log`,
`floor`, `ceiling`, `round`, `truncate`, `sign`, `cis`, `int8` ... `uint64` and `Duration.Complex`
now answer like Rakudo (they were "No such method" or `0+0i`).

Answers that moved toward Rakudo: `Date.Int`, `Numeric` and `Real` are the day count (they were the
POSIX timestamp) and `DateTime.Numeric` is the `Instant`, so `+$duration` stays a `Duration`;
`Date.mm-dd` and `yyyy-mm` exist; `DateTime.offset-in-minutes` is a `Rat`; `julian-date` and
`modified-julian-date` count from the UTC instant (a `DateTime` at `+01:30` was off by an hour and a
half); `Date.weekday`, `Date.Instant` and `DateTime.Int` are no longer answered.

Three places that read `.Numeric` as a number now take the `Bridge` of the object they get back: `==`,
the argument coercion of a builtin function and `sprintf`'s float directives, so a `Duration` still
formats as its seconds. The interpreter rows (`earlier`, `later`, `truncated-to`, `in-timezone`),
the objects group (`Mu`, `Code`, `Exception`, `Failure`, `Backtrace`, ...) and `RakuAST::*` are the
slice's remainder; the ADR lists each with its reason.
