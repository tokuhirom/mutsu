# `IO::Special.new` is a class-method row

`IO::Special.new("<STDOUT>")` was answered by a by-name gate on the interpreter's slow path. It is
now a class-method row found by owner for the `IO::Special` type object (ADR-11276 §9.197), next to
`Supply`'s class methods. Output is unchanged.
