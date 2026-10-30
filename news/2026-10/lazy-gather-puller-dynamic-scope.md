# A lazy gather body sees the dynamic variables of the frame that pulls it

`sub mk() { lazy gather { take $_ for dyn-list() } }` pulled inside a block that
declares `my $*DB` read `$*DB` as undefined in the body: the body ran in the
env the gather captured when it was created, which holds the creation call
chain's dynamics, not the puller's. Rakudo resolves a dynamic variable through
the scope that runs the body. Every force path (interpreter, VM strict force,
prefix pull and coroutine resume) now overlays the pulling frame's dynamic
variables on the gather's captured env (`inherit_puller_dynamics`). Red's
`ResultSeq.iterator`, which reads `$*RED-DB` inside such a body, needs this
(Red `t/91-cross-product`, part of #14094). Fixes #15112.
