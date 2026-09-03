# The positional-light call path binds its parameters from the stack

`todo/perf/late-august-call-path-slowdown-remainder.md` listed the args/locals
`Vec` churn as the largest remaining item on the call path (~11% of
`bench-fib`). This closes the args half of it.

## What it was

`exec_call_func_op`'s cached positional dispatch borrowed a pooled `Vec` for
the arguments, `extend`ed the drained stack slots into it, handed the callee a
`&[Value]`, and recycled the buffer afterwards. `call_compiled_function_positional_light`
then borrowed a *second* pooled `Vec` for the callee's locals and cloned each
argument from the first buffer into its parameter slot. Every call therefore
paid a `Vec::extend_trusted`, two pool round-trips, two per-element drop loops,
and a clone/drop pair per argument — and the clone existed only because the
buffer was recycled rather than moved from.

In a profile of `bench-fib` (fib(25), JIT on, P-core `cycles/u`) that showed up
as `Vec::extend_trusted` 3.2% and `recycle_locals` 4.1% of self time, plus the
`Vec::resize` folded into the callee.

## What it is now

The callee takes `args_base: usize` and binds directly out of `self.stack[args_base..]`,
truncating back to `args_base` on every exit path (both arity errors, the
parameter type-check failure, and normal completion). No intermediate buffer
exists at all: the hot dispatch site just computes the base index and calls.

Binding by move rather than by clone needed one restructuring. The parameter
type checks used to run interleaved with the binds, so moving a value out of
its stack slot would have left `Nil` behind for the `arguments` attribute of a
later parameter's `X::TypeCheck::Argument`. The checks now run as a separate
pass over the untouched stack slots first — using `Value::unwrap_varref`, which
borrows instead of cloning — and only then does the bind loop `mem::replace`
each argument into its slot. A failure still reports the complete, unmodified
argument list.

The old `&[Value]` entry point survives as a thin wrapper (it pushes the slice
and delegates) for the two cold call sites that hold an owned argument vector:
the OTF promotion arm and the slow `call_function` resolution path.

## Measurement

Interleaved A/B of two release builds, nine alternating runs each, median
retired user cycles on a pinned P-core:

| benchmark | delta |
| --- | ---: |
| `bench-tak` | −10.6% |
| `fib` | −7.2% |
| `bench-fib` | −6.7% |
| `method-call` | −0.6% |
| `bench-ctor` / `bench-class` / `bench-hash` / `hash-access` | ±1% |

Both orderings were measured; every sign flipped with the swap, so none of it
is drift. `poly-call` reads +1.9%, but a profile of the new binary samples zero
cycles in any changed function while running it — that is the known
binary-layout effect, discharged the way the ticket requires.

`t/positional-light-arg-consumption.t` pins the two invariants the design rests
on: the arguments are consumed on every exit path (so a surrounding expression
still sees a balanced stack), and a type-check failure on the second parameter
still reports the first one.
