# `.AST`: types exported by `use NativeCall` are types

`my constant CArray is export = NativeCall::Types::CArray` is now recorded by the
module scan as a constant holding a type object. A bareword naming it (`CArray`,
`Pointer`) renders as `RakuAST::Type::Simple`, and an index over it
(`CArray[uint16]`) as `Type::Parameterized`, matching rakudo 2026.11 (#18251).
