# A `--> CArray[T]` native return views C memory

A native routine declared `--> CArray[uint8]` (Crypt::LibGcrypt's
`gcry_md_read`) returned the raw address as an `Int`; it is now a `CArray`
over that address, so indexing and slicing read the C bytes. `Blob.new` /
`buf8.new` of a `CArray` slice take the bytes the slot references hold (they
were read as 0). `$buf.isa('Buf[uint8]')` (and `Blob[uint8]`) is True, so
Crypt::LibGcrypt's md/hash/cipher test files agree with Rakudo.
