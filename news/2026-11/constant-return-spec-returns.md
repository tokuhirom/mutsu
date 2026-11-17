# A constant return spec answers its type from `.returns`

A routine can declare a constant as its return (`sub f(--> 0) { }`). Its
`.returns` now answers the type of that value (`Int`, `Bool`, `Nil`), not
the value itself, as on rakudo (#18010).

- **Setting candidates.** A setting candidate (`&infix:<+>.candidates[0]`)
  now keeps its own `-->` spec: its signature prints `( --> 0)` where it
  printed `()`.
- **Qualified core enum values.** A value such as `--> Bool::True` or
  `--> Order::Less` is now recognised as a constant return, by the
  compiler and the runtime alike. `sub f(--> Bool::True) { }` now
  returns `True`; it returned `Nil` before.
