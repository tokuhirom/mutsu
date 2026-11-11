# `=:=` of a mixin type object against `Nil` no longer renders its name

`values_identical` decides a `Type but Role` type object against a plain
type object by comparing the mixin's composed name (`B+{M}`) with the other
side's -- and it rendered that name first, although only a composed class
name (one containing `+{`) can ever match. The common comparison is an
absence test against `Nil` or `Any` (Monad's `absent(Mu \candidate)` runs two
of them per construction), so every one of them built the base type's name
and the `+{...}` suffix for nothing. The other side's name is now checked
first. Refs #17331.
