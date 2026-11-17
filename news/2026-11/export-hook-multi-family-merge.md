# A `sub EXPORT` multi and an `is export` multi are one family

Two `use`s of a scope that bring candidates of the same `multi` now give the
name one dispatcher over all of them, as in rakudo. This holds even when one
module hands its candidates out from a `sub EXPORT` Map (`Map.new:
Mod::EXPORT::ALL::`, which is what `use Red:api<2>` does) and the other exports
its candidates with `is export`. Before, the later `use` replaced the earlier
one's candidates, so a call that only the earlier module's candidate accepts
died with `X::Multi::NoMatch`.

- A plain routine family: the binding each import set gives the name now
  includes the candidates of the dispatchers that earlier sets of the scope
  bound it to.
- A built-in operator family such as `trait_mod:<is>`: the hook's candidates
  join the dispatcher of whichever import comes second.

Red::Driver::Pg's `t/schema-constraints` imports Red's traits through the hook
and its own `is exclude` trait with `is export`. It now matches rakudo (10/10).
