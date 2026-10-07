# `IO::Handle.READ` and `.WRITE` are method-table rows

`IO::Handle`'s `READ(Int:D)` and `WRITE(Blob:D)` primitives now work on a real handle, as in Rakudo:
`READ` answers up to that many bytes as a `Buf`, `WRITE` writes the raw bytes and answers `True`, and a
wrongly typed argument fails the bind with `X::TypeCheck::Binding::Parameter`. Before, both died with
"No such method". They are rows of the one built-in method table (ADR-11276 slice 3E, part 3), and the
hand-written list of `IO::Handle` method names in `class_introspection.rs` now reads the table instead of
repeating it.
