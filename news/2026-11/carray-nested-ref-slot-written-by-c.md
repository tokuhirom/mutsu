# A nested `CArray[CArray[T]]` slot that C wrote reads back as a CArray

A `CArray[CArray[uint8]]` handed to C as an out parameter (OpenSSL's
`ASN1_STRING_to_UTF8(unsigned char **out, ...)`) kept answering the slot's element type object after
C stored an address in it. The reference-element read now boxes the address the same way `nativecast`
does (an unmanaged `CArray[uint8]` over the C memory). IO::Socket::Async::SSL's subject-alt-name
check no longer sees empty names, which unblocks its `t/ciphers`, `t/dh-ciphers` and
`t/ecdh-ciphers` (#14894).
