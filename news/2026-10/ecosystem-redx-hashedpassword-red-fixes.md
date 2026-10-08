# Red-based distributions: five interpreter fixes (RedX::HashedPassword)

Working RedX::HashedPassword's `t/020-basic.t` (a Red model with an `is password` column) exposed
a chain of independent interpreter gaps, fixed together:

- `Attribute.build` answers `Mu` for an attribute without an initializer (it answered the
  parser's type-object seed as a closure for `has Int $.id`) and reflects `set_build`.
- `$obj.^meth(...)` hands an instance to a *user-defined* metaclass as the instance; only the
  builtin metaclasses get the type object.
- A `Proxy` argument is FETCHed before a typed (also `is raw`) parameter's type check; the
  positional light-call binders decline Proxy arguments.
- A `Proxy` passed as a named constructor argument is FETCHed as it lands in the attribute, on the
  native and the interpreter construction paths.
- A role's `multi method f($value)` is no longer replaced by the class's `multi method f(@value)`
  (and `%value`): the container sigil is part of the candidate signature.
