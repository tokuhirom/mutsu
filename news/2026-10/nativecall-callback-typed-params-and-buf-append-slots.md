# NativeCall callbacks receive typed parameters; Buf.append takes CArray slots

Two gaps found running Archive::Libarchive::Raw (all 7 test files now match
rakudo):

- A `&callback (...)` parameter whose signature names a `repr('CPointer')` class
  (`archive`) or a `CArray[T]` now hands the Raku callable an instance of that
  type viewing the C pointer, as rakudo does, instead of a bare `Pointer`
  (which failed the callable's own type constraint: "expected archive but got
  Pointer").
- `Buf.append` / `push` / `unshift` / `prepend` fetch Proxy elements, so
  `$buf.append: $carray[^$n]` appends the bytes instead of zeros (a
  `CArray[uint8]` slot is a `UIntPosRef` Proxy).
