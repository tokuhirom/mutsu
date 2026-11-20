# `.AST`: a constant aliasing a parameterized or imported type is a type

`constant A = Array[Int]` and `constant CA2 = CArray` (an imported NativeCall type) now
make the later word `A` / `CA2` a `Type::Simple`, and `CA2[uint16]` a
`Type::Parameterized`, matching rakudo 2026.11 (#18252).
