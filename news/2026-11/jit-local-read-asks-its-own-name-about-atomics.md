# The JIT's inline local read asks its slot's own name about atomics

The JIT reads a plain local as two loads and a store, guarded by a process-wide
latch (`LOCAL_READ_SPOILERS`) that is zero only while no spoiler exists. One of the
spoilers is "an atomic variable was registered", and it is a fact about a NAME: the
interpreter's own `GetLocal` arm already asks it of the slot's name
(`atomic_name_possible`). The emitted code could not, so a single `my atomicint`
anywhere in the process (a loaded module such as Cro declares one) sent every inline
local read of every compiled routine through the `get_local` shim for the rest of the
run.

The slot's name is known when the code is compiled, so the emitted read now keeps the
one-load fast path while the latch is zero. Once it is non-zero it asks which kind of
spoiler it is: one that applies to every variable (a `$CALLER::x :=` alias, a
sigilless attribute alias) still sends the read to the shim, and an atomic variable
sends it there only when the bit of this slot's own name in the atomic-name set is
set (`Interpreter::atomic_name_probe` resolves the word's address and the mask at
compile time). A name registered after the compile sets that very bit, so a routine
compiled earlier still reads its own variable correctly
(`t/concurrency/thread-lock/jit-local-read-with-atomic-elsewhere.t`).

A plain `sub` call in a `for` loop with `use Cro::HTTP2::RequestParser` loaded
(profiling build, callgrind slope over 3,000 to 9,000 iterations): 7,134 -> 6,209 Ir
per iteration together with the operator-site memo of #16565; without the module
5,518 -> 5,515.
