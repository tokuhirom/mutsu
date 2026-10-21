# NativeCall Pointer globals retain the symbol address

`cglobal` with a `Pointer` or parameterized `Pointer[T]` target now returns the
symbol's address, matching Rakudo for both functions and variables. Numeric
targets still read the value at that address. Callers can explicitly dereference
a pointer target when they need the contents of a variable.

This removes an extra dereference that interpreted function machine code as an
address, crashing bindings such as Gnome::Glib when they cast the result to a
callable signature.

Parameterized Pointer targets are now accepted by `nqp::nativecallglobal`, and
`Code.returns` and `Code.of` honor a signature installed through `$!signature`.
This lets upstream NativeCall marshal the declared return type of a function
whose signature was constructed at runtime.
