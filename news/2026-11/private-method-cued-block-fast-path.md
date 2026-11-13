# A cued block calls its class's private methods on the compiled path

A block that closes over `self` and runs in another thread now calls the class's private methods
(`self!helper(...)`) on the compiled method path. Two examples are a block passed to
`$*SCHEDULER.cue` and a `start` block. The fast-path permission check used to ask only which
method was running on the current thread. A block running in a scheduler fork has no such frame,
so every one of those calls took the interpreter fallback. The fallback then allowed the call by
the class of the lexical `self`. The check now asks the same lexical-`self` and lexical-package
questions as the general private dispatch, so it allows exactly the same calls.

ParaSeq pays this twice per batch (`self!cue` and `self!batch-done`). With the change, a degree-3
`.hyperize(16, 3, :!auto)` map takes about 420 us per batch instead of 460 us.

The `nqp::atpos`/`elems`/`push`/`shift` lane for an `IterationBuffer` also accepts a buffer held
in a captured or bound variable. Such a buffer arrives as the variable's cell, which every op
already reads through.
