# Binding a parameterised type object into a same-typed lexical

`my Positional[Int] $z := $x` (and `Associative[Int]`) no longer fails the
binding type check. The `:=` source arrives wrapped as a VarRef, which the
constraint check inspected instead of the value it carries, and the
`Positional`/`Associative` parameterised arms rejected a `ParametricRole` type
object instead of deferring to the role-argument comparison.
