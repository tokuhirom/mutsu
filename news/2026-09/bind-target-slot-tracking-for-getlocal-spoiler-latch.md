# Compile-time bind-target-slot tracking laid for the `GetLocal` spoiler latch (#8748)

[#8748](https://github.com/tokuhirom/mutsu/issues/8748) reported that the
interpreter's `GetLocal` fast path (#8332) and the JIT's Tier B inline local
read (ADR-0004 J4d) both gate on `crate::vm::vm_jit::LOCAL_READ_SPOILERS`, a
process-global, monotonic, never-decremented `AtomicU32`. Packing a single
`ContainerRef` cell anywhere in the process — one `my $x := $y` on line 1 of
an unrelated file — disables the fast local read for every slot, in every
frame, for the rest of the process, even for locals that are plain `Int`s
with no cell, no alias, and no relation to the bind at all. Measured cost on
the issue's repro: +21.7% instructions retired with the JIT on, +11.6% with
it off, on a hot loop that never reads the spoiled variable.

A sound fix needs a per-slot answer to "could this local ever become a
`ContainerRef`/`Proxy` word", replacing (for that one question) the
process-wide latch. Investigating what the compiler already knows found the
raw material already exists but was never recorded: every `:=`-shaped
compile site resolves and emits the exact target local slot via
`OpCode::TagContainerRef`'s `source_slot` field, it just discarded it after
emission. The same investigation also found the existing per-name tracking
(`CompiledCode::scalar_bind_locals`) was itself incomplete — it only covers a
**scalar** `my $x := ...` declaration, missing both **array/hash** binds
(the issue's own `my @unused := @data;` repro shape) and a **statement-level
rebind with no `my`** (`$x := $y;`, which emits no `TagContainerRef` at all
and was invisible to any existing tracking).

This lands the data-collection half only, following the same "no behaviour
change" precedent as
[ADR-0097](../../docs/adr/0097-a-binding-descriptor-addressed-by-slot.md)
slice 1: a new `CompiledCode::rebind_target_slots: Vec<u32>`, fed from all
eight `TagContainerRef`/`TagContainerRefReversed` emission sites plus the two
previously-uncovered declaration and no-`my`-rebind paths, and a memoized
`CompiledCode::local_may_be_celled(idx)` accessor mirroring
`local_read_plain`'s existing per-chunk memoization pattern. Five new unit
tests (`opcode::local_may_be_celled_tests`) pin the exact #8748 repro shape:
an unrelated array bind marks only its own slot, a program with no `:=`
anywhere marks nothing, and both previously-uncovered rebind shapes are
tracked correctly.

Neither new field is read by any execution path yet. Wiring
`local_may_be_celled` into the fast-path gate is deliberately deferred:
closure capture of a mutable outer lexical is a second, distinct source of
celling this analysis does not cover (a true closure boundary reaches outer
names through the upvalue mechanism, not the compiling chunk's own
`local_map`), and ADR-0097 §1.5 already records two prior instances of a
similar "make store logic per-slot" attempt shipping a subtle correctness
bug caught only by an existing test, not by review. ADR-0097 §11 records the
full investigation, the residual sources that must stay on a dynamic latch
regardless (`$CALLER::x := ...` aliasing is inherently name/dynamic-scope-based,
not slot-addressable from the compiling chunk), and the concrete next steps
— auditing closure capture, and extending the existing
`exec_get_local_op_inner` debug-assertion cross-check to verify
`local_may_be_celled` against runtime reality before it gates anything in
release.
