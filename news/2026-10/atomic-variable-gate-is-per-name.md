# One `atomicint` no longer slows every other variable's read

Declaring a single `atomicint` anywhere in a program latched a process-wide
"an atomic exists" flag, and from then on every `GetLocal`, every typed
declaration and every plain scalar store of every variable took the name-keyed
atomic cascade: a `format!` of the `__mutsu_atomic_name::<name>` key, an intern
of it, two type-constraint probes and a shared-store read. The interpreter's
`GetLocal` fast path was lost for the whole process too. That is most of why a
`cas` retry loop costs ~45k instructions per iteration where a non-atomic loop of
the same shape costs ~17k (#12120).

The question is now asked of the name. `runtime::atomic_names` keeps a
monotonic 1024-bit set of the names registered as atomic, marked at the only
places that register one (an `atomicint` constraint, and the legacy lane's
`atomic_value_key_for_name`), and `Interpreter::atomic_name_possible(name)`
reads it. A clear bit proves no `atomicint` constraint and no lane mapping
exists under that name, so the read, the typed store and the lane reset skip
the cascade for every unrelated variable. Two names may share a bit and then
simply take the old path. The bit is derived from a hash of the name's bytes,
because the read sites hold a `&str` and must not intern. The JIT's inline
`GetLocal` still reads the total spoiler counter; the interpreter's fast path
reads a second counter without the atomic source and asks the name instead.

Measured on the lexical `cas` loop from `roast/S17-lowlevel/cas-int.t`
(4 threads x 10000 iterations, callgrind, release): 1,831,043,453 ->
1,366,469,417 instructions, 45.5k -> 33.9k per iteration. The whole file went
from 7.97 s to 5.75 s on a 4-core container (five alternating runs each, both
sides incremental release builds). That is not the #12120 goal (within 1.5x of
rakudo, which runs the file in 1.5-2.6 s on the same box): the same loop with no
atomic at all costs ~17k instructions per iteration, so the typed per-iteration
`my int` declaration and the loop machinery are now the larger part of what is
left, and the `cas` call itself (~9.9k) is the next slice.
