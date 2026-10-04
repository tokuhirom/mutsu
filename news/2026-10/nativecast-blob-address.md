# nativecast of a Blob answers the storage address

`nativecast(Pointer, $blob)` used to build a NULL `Pointer` because `value_c_address`
only knew an `address` attribute, a native `CArray` and a native-storage `Array`. A
`Blob`/`Buf` instance now answers the address of its own storage, the same pointer a
`Buf` parameter hands C, so `memchr(nativecast(Pointer, $blob), ...)` no longer crashes
with a NULL dereference (#11754).
