# RakuAST round trip: regex subrule arguments, term constants and `$@`

Four more constructs now cross the RakuAST boundary and back (the round-trip
frontend ratchet grows by six files):

- Subrule arguments written as postfix updates (`<x($c++)>`), assignments
  (`<x($s = 7)>`), block calls (`<x({ ... }())>`) and block-literal hashes
  (`<x: { key => <v a l> }>`) are re-spelled from the lowered expression, so
  the argument source the regex engine re-reads matches the parsed program.
  A list of plain words is spelled as a `<a b c>` word list.
- `constant \term:<ℵ₀> = ...` lowers back to a declaration under its
  `term:<...>` name, and a later bare `ℵ₀` converts to `Term::Name` of that
  declared name, as rakudo does.
- `$@` / `$%` (item contextualizer over an anonymous variable) lower back.
