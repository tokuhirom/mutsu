# A method assigning an outer variable no longer inherits its caller's readonly param

`readonly_vars` is keyed by bare name and follows the dynamic call stack, so a
class method that assigns an outer `my $v` died with "Cannot assign to a
readonly variable or a value" whenever the routine calling it had its own
readonly `$v` parameter (#11054). Closures and nested subs already recorded the
readonly state of their written free variables at creation (#10389, #10400);
methods had no code object to carry that record.

A `MethodDef` now carries `captured_readonly`, a snapshot of the declaring
frame's readonly marks taken when the class, role or `augment` body registers
the method (the compiled body may not exist yet, so the whole snapshot is kept
and narrowed at call time). Both compiled method entry paths call
`reconcile_method_readonly`, inside the call's journaled readonly frame, which
puts each written free variable back into the declaring frame's state. Only a
parameter/loop-alias mark is dropped when the snapshot lacks the name; an
immutable-kind mark (`$x := 42`) describes the binding itself and may be made
after the class registered, so it is kept.

Found via the `P5print` distribution's `t/01-basic.rakutest`. The same bug for a
top-level sub called through a code value is #11070.
