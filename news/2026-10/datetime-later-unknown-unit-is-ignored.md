# `DateTime.later`/`earlier` ignore a unit they do not know

`DateTime.now.later(:1sec)` died with "Unknown unit 'sec' for DateTime.later". rakudo does
not raise for a unit it does not know (`:sec`, `:secs`, `:min`, an invented `:frobs`): the
DateTime comes back unchanged. mutsu now does the same; the real units (`:second`/`:seconds`,
`:minute`, `:day`, ...) are unaffected.

Red's `t/27-LPW-2019` writes `.later(:1sec)`.

`Date.later` with an unknown unit still raises, with its own message rather than rakudo's
`Type check failed for return value; expected Dateish:D but got Slip (Empty)`.

Refs #14094.
