# `new_type` on user HOW subclass instances

Instances of user-defined subclasses of `Metamodel::ClassHOW` now inherit
`new_type`, matching calls on the built-in HOW instance. Types created through
that method keep the user HOW subclass as their `.HOW`.
