# A closure's captured variable no longer inherits its caller's readonly parameter

`Interpreter::readonly_vars` is keyed by bare name and follows the dynamic call stack. A readonly
(non-`is rw`) parameter `$string` of the routine that *called* a closure was therefore still marked
while the closure ran and assigned its **own** captured `$string`, which failed with "Cannot assign
to a readonly variable or a value" (`$string++` failed with "Cannot resolve caller
postfix:<++>(string); the parameter requires mutable arguments"). The topic `$_` already had a
dedicated reset on routine entry; every other name did not. It surfaced in
`Terminal::ANSIParser`, whose parser callbacks assign their own `my Buf $string` while the test
helper driving them has a `$string` parameter.

The code object now carries the answer instead of the registry guessing it
(`SubData::captured_readonly`). Creating a closure, or the code object `&name` of a routine declared
inside another routine (`my sub`, nested `sub`), records which of the scalar free variables its body
writes were readonly in the creating frame (`Interpreter::capture_readonly_state`). Entering it
reconciles the registry against that record (`Interpreter::reconcile_captured_readonly`), inside the
call's readonly frame, so the caller's marks come back on return.

Because the record is the creating frame's state, the rule now holds in both directions: an
unrelated caller parameter no longer blocks a write, and a closure over a routine's *own* readonly
parameter stays readonly after that routine returned (`sub mk($x) { { $x = 1 } }`), as in Rakudo.

Not covered: a nested `my sub` called *by name* from an escaped closure that does not itself write
the variable (the closure has no record to reconcile from); it is tracked in
[#10400](https://github.com/tokuhirom/mutsu/issues/10400).
