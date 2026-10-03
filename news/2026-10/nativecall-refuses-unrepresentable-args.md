# NativeCall refuses arguments it cannot represent

A defined argument that the native marshaller could not represent as the
declared `CArray` or `Blob` parameter used to become an empty per-call buffer.
That buffer's pointer is dangling but not NULL, and it was handed to C as is.
`frexp(8e0, "abc")` against `sub frexp(num64, CArray[int32] --> num64) is
native` crashed mutsu with SIGSEGV (#11529). Memory safety is a trust boundary
in `docs/security.md`: nothing a Raku program does may corrupt memory.

Such an argument is now refused with rakudo's catchable error:
`Native call expected argument 2 with CArray representation, but got a
P6opaque (Str)`. A `Blob` parameter reports `VMArray` representation.

Empty storage is now a NULL pointer, as MoarVM's empty storage is. This
covers an empty `CArray`, an empty `Buf`, and an empty Raku array passed for a
`CArray`. Before, it was the dangling pointer of an empty allocation. A
`Blob` type object passed for a `Blob` parameter is NULL too, which matches
how an undefined `CArray` argument was already handled.
