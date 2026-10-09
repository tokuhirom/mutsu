# Mutating overrides on `is SetHash`/`is BagHash` subclasses now mutate when they defer

A user method on an `is SetHash` / `is BagHash` / `is MixHash` subclass that ends in
`callsame` (`set`, `unset`, `add`, `remove`, `grab`, ...) used to answer without changing the
instance, because only `ASSIGN-KEY`, `DELETE-KEY` and `STORE` were routed through the mutating
method rows. Every name now tries the `Handler::Mut` row on the backing storage first and falls
back to the pure entry when it has none (#12412).
