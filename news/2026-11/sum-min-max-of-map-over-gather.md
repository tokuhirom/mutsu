# `sum`, `min` and `max` of a `.map` over a gather read all the elements

`sum(gather {...}.map({...}))` answered 0 and `max` answered `-Inf`: the pipe's
elements live in the gather's coroutine, and the pure routines were handed the
unforced lazy list. A finite lazy argument of `sum`/`min`/`max`/`minmax` is now
forced first. Math::Sequences' `necklaces` (`$n R/ sum divisors($n).map: ...`)
is correct again.
