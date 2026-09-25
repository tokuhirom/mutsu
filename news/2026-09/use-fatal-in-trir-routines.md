# `use fatal` holds inside a routine with parameters

Under `use fatal`, a Failure passed as an argument must throw at the call
site. mutsu got that right in a routine without parameters, and wrong in one
with parameters (#9453):

```raku
use fatal;
sub mk($b) { $b ?? Failure.new("boom") !! 1 }
sub h($) { 1 }
sub callr($b) { h(mk($b)); "no-throw" }
say (try { callr(True) }) // "threw";   # Rakudo: threw   mutsu: no-throw
```

The cause was not the binder. A routine with parameters is eligible for TRIR,
the typed IR, and TRIR runs its body with its own call ops (`CallTr`,
`CallGen`, `MethodGen`). Every interpreter call arm runs the `use fatal`
argument check (`explode_if_fatal_failure_in_call_args`) before it
dispatches. The TRIR call ops never did, so the Failure was bound to `h`'s
parameter instead of being thrown. With `MUTSU_TRIR=off`, the repro threw.

The check now takes its arguments as a slice
(`explode_if_fatal_failure_in_arg_values`). The interpreter arms and the three
TRIR call ops all run it. A TRIR site reads its object arguments in place,
from the frame's slots and the top of its object stack. Native arguments are
machine integers and can never be a Failure. Whether `use fatal` applies no
longer depends on which executor runs the calling routine.

Regression test: `t/exceptions/failure-fatal-mode-call-argument.t`. It covers subs with
no, one, several and typed parameters, and methods, both calling a sub and
calling a method with the Failure.
