# A sigilless bind's writability verdict travels on the stack

`my \x := EXPR` is writable when its bind source denotes a container
(`@a[0]`, a `$` variable) and immutable when it is a plain value (`5`,
`$s.uc`). The source is only on the stack before the declaration's store, but
the readonly marker can only be written after it, so `MarkSigillessBindSource`
used to park its verdict in `Interpreter::sigilless_bind_source` for the
`MarkSigillessBind` that followed.

Now the verdict is a `Bool` the source op leaves just below the source
(`[v] → [verdict, v]`), and the compiler tells the paired
`MarkSigillessBind { from_stack: true }` to pop it. The bound-term store lane's
"plain value" question is the new `BOUND_VALUE` mark-context bit. A declaration
nested in another's initializer, a store re-entering user code, or a loop can
no longer meet someone else's verdict. ADR-10779 D3: `Interpreter` loses a
field.
