# Leave backtraces off unthrown exceptions

An exception created by `fail` now keeps its fail-site origin on the Failure
without marking the exception as thrown. Reading `.exception.gist` before a
throw shows only its message, and `.backtrace` is undefined on newly created
exceptions. Throwing the exception attaches a Backtrace at that point. Sinking
the Failure still reports the original fail-site frames.
The array shift complexity test now compares the fastest of three timing
samples at each size, preserving its linear-growth threshold under scheduler
pauses.
