# A lazily-read gather keeps every value a hyper `».take` produces

`(gather { @a».take }).map({ $_ })` yielded only the first element (#9785).
A hyper method call runs its whole element loop inside one opcode, and the
lazy gather driver can resume a suspended body only at an instruction
boundary: the take-limit signal raised by the first `.take` unwound the op,
and the driver resumed past it, losing the other elements.

Hyper method calls (`HyperMethodCall`, `HyperMethodCallDynamic`) and hyper
sub calls (`HyperFuncOp`) now run with take-limit suspension deferred to the
end of the op. A take reaching the pull's limit inside them only parks the
existing `gather_suspend_pending` flag; the op finishes its (finite) element
loop, and the gather suspends right after it, stamped with the op's location
the same way an `OpCode::Take` is. An enclosing `for` loop therefore resumes
the same iteration after the op, and a `while`/`loop` body keeps suspending at
its iteration boundary.
