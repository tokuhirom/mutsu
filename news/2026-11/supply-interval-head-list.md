# `Supply.interval(..).head(N).list` collects the emitted values

A Supply derived from a timed `Supply.interval` by `.head(N)` carries neither a
channel nor values of its own, so `.list` answered `()` and `.Array` `[]`. They
now tap the derived supply and collect what it emits until it is done, as
`(0 1)` in Rakudo. Stacked stages (`.map(..).head(N)`, `.head(N).map(..)`) and
`.head(N).Seq` still drop their values; that is tracked separately.
