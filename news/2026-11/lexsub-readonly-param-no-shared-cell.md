# A nested sub reads its routine's readonly parameter without a shared cell

A sub declared inside a routine (`method lim(&by, int $direction) { sub
processor($input) { $*SCHEDULER.cue: { ... by(...) ... $direction ... } } }`)
reads the routine's variables through per-activation aliases (mutsu#9111), and
each free variable used to be boxed into a shared `ContainerRef` cell. For a
readonly signature parameter (no `is copy`, `is rw` or `is raw`) the cell bought
nothing — the binding can neither be assigned nor rebound for the whole
invocation, and rakudo binds it to the argument's value — but every read of it
locked the cell. ParaSeq's limiter cues one task per batch, and every worker
read the same `&by` and `$direction` cells per element, so the workers
serialized on one lock (#17337).

The alias of such a parameter now holds the value itself, and a closure the
nested sub creates freezes it into its upvalue array, exactly as a closure
created directly in the routine already froze a readonly parameter (ADR-12529
§2.1). On a three-task copy of the limiter loop the wall time drops from
0.95 s to 0.64 s.
