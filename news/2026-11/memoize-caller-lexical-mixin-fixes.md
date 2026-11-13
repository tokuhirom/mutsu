# Memoize runs: caller lexicals, routine mixins and qualified Hash methods

`Memoize`'s suite (`t/memoize.rakutest`) died on its first test and now passes
all 20, matching raku. It exercised five separate divergences, each fixed:

- `CALLER::LEXICAL::` now lists a `sub` the caller declared inside a block as
  `&name` (the env held only its internal escape-hatch key), so
  `memoize('a')` finds the caller's `sub a`.
- A wrap handle kept in a `:=`-bound attribute unwraps; the handle arrived in
  its container and was rejected as "invalid wrap handle".
- A role composed into a routine through another reference (`&code does R`
  in a callee) is seen by a later method call on the routine: the call takes
  the routine's current composition (ADR-11827), so an accessor named like a
  built-in method (`.cache`) reaches the role.
- `self.Hash::EXISTS-KEY(key)` from a role mixed into a Hash calls the Hash's
  own method instead of re-entering the role's override forever.
- Binding a Hash with a role mixed in to a `%` attribute keeps the role and
  the Hash itself instead of a plain copy.
