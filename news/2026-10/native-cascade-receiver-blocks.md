# The native cascades' dead receiver blocks are gone

The 0- and 1-argument native cascades no longer carry method arms, only
receiver blocks: a `.VAR` descriptor's deferral, `Failure`'s answers, the
`Date`/`DateTime` subclass owner rows, a buffer's `.values`, `Backtrace`'s
answers, the positional list coercions, and the 1-argument blocks for an
`Int` subclass, an allomorph and a `Match` owner. The same liveness measurement
that cleared the native entry's bypasses (log which test file reaches each
block, rerun those files with the block skipped, compare against `raku`) found
that none of these changes an answer: the rows answer each of these receivers
before the cascade is reached. They are deleted, with the two table entry points
only they used. `t/oo/method/builtin-method-cascade-receiver-answers.t` pins the
answers.
