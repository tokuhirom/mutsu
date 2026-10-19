# `.^can` on a custom HOW sees methods from parents added during `compose`

When a user subclass of `Metamodel::ClassHOW` adds a parent while composing a class, `.^can` and
`HOW.can` now inspect that class's MRO. The generic instance `.can` handler no longer mistakes the
type-object argument to the inherited ClassHOW method for a method name.
