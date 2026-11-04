# `protocol`-style SubsetHOW declarators, and `Method.yada`

A `Metamodel::SubsetHOW` subclass mapped through `EXPORTHOW::DECLARE` (Protocol's `protocol Writer {
method write { ... } }`) calls `callwith :$name, :refinee(Any), :refinement{ ... }` from its
`new_type`. The native base only returned the type object under registration, so the refinement was
dropped and `W ~~ Writer` was always False. The refinement is now registered as the declared name's
subset, as `subset Writer of Any where { ... }` does. `Method` objects from `.^methods` and
`.^lookup` also answer `.yada` (a `...` body), which Protocol's `compose` uses to reject non-stub
methods. Protocol's `t/01-protocol.rakutest` is 19/19, like raku.
