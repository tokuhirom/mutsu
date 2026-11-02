# An empty `CArray[CArray[uint8]]` as a C out-parameter no longer crashes

`CArray[CArray[uint8]].new` passed to a C function that writes through it (`char **endptr`,
OpenSSL's `unsigned char **out`) crashed: the empty array's storage was handed over as NULL. An
empty array of references now gets one zeroed pointer slot for the call, which C can write through; the
slot stays as the array's element when C wrote an address into it and is taken back otherwise. The
parameter's element type is also read from a `CArray[CArray[T]]` signature (the type object is a
mixin, whose parameterized name was not consulted), so the nested array is known to hold pointers
(#15985).
