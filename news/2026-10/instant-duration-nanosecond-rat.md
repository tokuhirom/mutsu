# Instants and Durations hold nanosecond Rats, like Rakudo

`now - now` used to build a Duration holding a Num, `now` itself was a
Num-valued Instant, and `Instant.from-posix(1/3)` went through a float. Rakudo
stores every Instant's and Duration's TAI seconds as a Rat truncated toward zero
to whole nanoseconds (`Duration.new(1/3).tai` is `333333333/1000000000`). mutsu
now does the same everywhere they are built: `now` reads the clock at
nanosecond resolution into an exact Rat, `Instant.from-posix` adds the
leap-second offset exactly, and `Duration.new`, `Instant ± Real`,
`Instant - Instant`, `Duration ± Real` and `Duration % Real` compute on the
stored Reals and store the result through one shared helper
(`arith::tai_rat`). The Num-to-Rat overflow that `(now - now).narrow` once hit
can no longer be reached through these paths (#11273).
