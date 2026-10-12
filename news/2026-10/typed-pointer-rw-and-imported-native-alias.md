# Typed `Pointer[T] is rw` out-parameters get their address; imported native aliases default to 0

Found running Font::FreeType's suite:

- A typed `Pointer[T] $p is rw` argument is a mixin around the Pointer instance; the written-back
  address now lands in the wrapped instance (the untyped `Pointer is rw` already worked).
  `FT_Init_FreeType(Pointer[FT_Library] $lib is rw)` previously left the pointer null.
- `my MyByte $x;` where `MyByte` is a constant alias of a native type that the compiler could not
  see (imported from a module) no longer dies with "Cannot unbox a type object (Nil)" when it has
  no initializer.
