# A sunk `.map` in a lazy gather suspends at each `take`

`lazy gather { (1..*).map: { take $_ } }` died with `X::Cannot::Lazy` because the
sunk map was forced whole. When a bounded pull of a gather reaches a `SinkPop`
of a lazy map/grep pipe, the op now pulls the pipe one element at a time and
suspends between elements (`ForLoopResumeState::SunkPipe`), leaving the pipe on
the saved stack so the next pull continues it. It applies outside any loop op of
the gather body; a sunk map inside a loop, or in a `try` body that is not sunk,
keeps its previous behaviour. Math::Sequences' Bell numbers (A000110) are the
motivating case. Closes #17071.
