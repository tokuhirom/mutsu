# RakuAST round trip: lone `||@list` and deferred `X::Obsolete`

- A regex that is a lone sequential interpolation (`/ ||@list /`) re-spells its
  leading `||` when the pattern source is rebuilt from the tree, so the array's
  branches are tried in order rather than by longest match.
- `"${$scalar}"` / `"@{@array}"` inside an interpolating string defer their
  `X::Obsolete` into the unit as a `die`; the conversion now raises that
  exception itself (rakudo throws it while compiling) and the round-trip mode
  passes a unit's own compile error through instead of wrapping it as a
  conversion gap.
