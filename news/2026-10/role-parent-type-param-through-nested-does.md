# A role parent that is a type parameter survives nested role composition

Two gaps in role-in-role composition, found running Data::Record (`#15377`):

- `role P[::H] is H` composed as `P[Metamodel::ClassHOW]` gives its class the
  `Metamodel::ClassHOW` parent, but composing it through another role
  (`role Q[::H] does P[H]`, `class D does Q[Metamodel::ClassHOW]`) dropped
  that parent: the nested role's parents are now propagated with the type
  parameters bound by the outer composition, so `D.new_type(...)` mints a type
  whose HOW is an instance of `D` (with the role's attributes and BUILD).
- `does R[Array, Foo::Bar]` inside a role was taken for a forwarded `::T`
  capture because the argument text contains `::`, which left `R`'s type
  parameters unbound in its submethods (`Undeclared name: T`). Only a `::`
  that does not continue an identifier forwards a parameter now.

Still open on Data::Record: its `annotations::containers` `Buffer` (a
`Positional` with a user `STORE` bound to an attribute and assigned a list)
(#15259), and forwarding `role Q[::U] does R[U]` into `R`'s BUILD.
