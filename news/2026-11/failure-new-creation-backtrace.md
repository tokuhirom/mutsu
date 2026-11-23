# `Failure.new` records where it was created

A `Failure` built with `Failure.new` carried no backtrace of its own: its
`.backtrace` was empty, and an unhandled one rendered the place it was finally
thrown as if that were its origin. `Failure.new` with an exception that was
never thrown now records the backtrace of its creation, as rakudo does: the
stack without the frame that called `Failure.new`, so a top-level
`Failure.new` has no frames and its uncaught report lists the throw site under
"Actually thrown at:" (#18987). An exception that was already thrown keeps its
own backtrace, and `fail` is unchanged.
