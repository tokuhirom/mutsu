# Archive::SimpleZip writes zips: cast CArrays, `Buf[uint8]($x)`, pipe `.lines`

Archive::SimpleZip 0.8.0's `t/002-basic.t` died in its second subtest. Three
interpreter gaps stood in the way, each now fixed in general:

- A `nativecast(CArray[uint8], $blob)` result -- an unmanaged CArray viewing
  the Blob's bytes -- was refused by a native `CArray[...]` parameter
  ("expected argument 2 with CArray representation, but got a P6opaque"). It
  now passes its address, as MoarVM's CArray REPR does, so the module's
  zlib `crc32` call works. The cast object also keeps its source alive, so a
  cast of a temporary (`nativecast(CArray[uint8], "abc\0".encode)`) no
  longer reads freed memory.
- Calling a parameterized buffer type coerces, like calling `Buf` does:
  `Buf[uint8]($utf8)`, `Blob[uint16]((1, 2))`. A buffer already of the target
  type is returned itself (`Blob[uint8]($utf8) =:= $utf8`).
- `.lines` on a process's output pipe is a `Seq`, as on any `IO::Handle`, so
  a `--> Seq` routine can return it.

With these, all four of Archive::SimpleZip's test files match rakudo.
