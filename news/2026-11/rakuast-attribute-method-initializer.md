# RakuAST: attribute defaults carry Rakudo's `Method::Initializer` thunk

An attribute declaration now has the shape rakudo 2026.11 gives it (#18621):

- **Literal default** (`has $.x = 5`, `= "s"`, `= True`): `Trait::WillBuild`
  holds the literal itself. The declaration visits an empty
  `Method::Initializer` between its `initializer` and its traits.
- **Any other default** (`= 1 + 2`, `= -1`, `= [1]`): `Trait::WillBuild` holds
  a `Method::Initializer` thunk with a `(Mu $_)` signature whose body returns
  the expression. The declaration visits the thunk and not its initializer.
- **`.=` default** (`has Int $.r .= new(3)`): the thunk calls the method on the
  declared type.
- **Class attribute** (`my $.q`, `our @.p`): there is no `WillBuild`. The
  declaration visits a `Method::ClassAccessor` that returns the variable.

These generated nodes have no origins, and their signatures visit the
implicit invocant and `%_` the way a method's does. The expression inside a
thunk keeps the origins of what the source wrote. `.raku`, equality and the
`EVAL` round trip still show only what the source wrote.
