# A `:$var` argument's VarRef skips the declared-type lookup

`WrapVarRef` tags an argument variable with its name and resolves the
variable's declared type in its own frame (ADR-13757), for the binder that may
read it later. The value of a named argument `:$var` never reaches a binder as
a VarRef: the next op, `MakeNamedArg`, turns it into the variable's cell (or,
for a lazy argument, the call site does), and that capture looks the
constraint up by name in the caller's frame. So for a `WrapVarRef` followed by
`MakeNamedArg` the declared type is left unresolved, which saved an env record
probe for a typed variable.

On #19384's `Quoted.new` reproducer (two typed `:$var` arguments) `WrapVarRef`
went from 1.42k to 1.06k instructions per call (callgrind, profiling build).
Part of #19399.
