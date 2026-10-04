# Role multis, role protos and delegating start methods (IP::Addr)

Working the `IP::Addr` ecosystem distribution exposed four interpreter gaps, all fixed generally:

- A class's own `multi method set(:$b!)` no longer replaces a composed role's `multi method set(:$ip!)`.
  The composition de-duplication compared only positional signatures; a multi's named parameters are
  part of its dispatch signature too.
- A role's `proto method` body now runs for the composing class (it used to be dropped, so
  `proto method set(|) { {*}; self }` returned the multi's value instead of `self`).
- `method TOP { ...; self.rule }` returns the delegate rule's Match, so that rule's action now fires.
- `Grammar.parse(:args(:validate))` (a lone Pair) passes the start rule no arguments, like rakudo,
  instead of one positional.

Tests: `t/oo/role/role-multi-named-compose.t`, `t/grammar/grammar-method-start-delegate-actions.t`.
