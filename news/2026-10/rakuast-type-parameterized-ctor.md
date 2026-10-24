# RakuAST::Utils: hand-built `Type::Parameterized`, `Signature` EVAL, typed-container roles

The `RakuAST::Utils` distribution's suite now passes 7/7 (it was 2/7). Four gaps closed:

- `RakuAST::Type::Parameterized.new(base-type =>, args =>)` is constructible and lowers.
- `RakuAST::Signature.new(...).EVAL` yields the `Signature` value, as `:(...)` does.
- A constructor argument read from an array element (`Name.from-identifier(@parts.head)`) is stored
  by value instead of as its container cell.
- `nqp::can($type.HOW, "roles")` is true (a meta-object answers its own metamethods), and
  `Array[T].^roles` / `Hash[V].^roles` / `Hash[V,K].^roles` report rakudo's composed roles
  (`Array::Typed[T] Positional[T] Positional Iterable`, ...), which `TypeAST` uses to see a
  parameterization.
