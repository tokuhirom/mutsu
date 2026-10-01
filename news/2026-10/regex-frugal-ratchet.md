# Frugal quantifiers under ratchet grow on demand, and compile

In Raku a frugal quantifier keeps its frugality under `:ratchet` (in a `token` or `rule`): it tries
the shortest admissible count first and grows one iteration at a time until the rest of the pattern
matches; ratchet only commits each iteration's atom to its first match. mutsu's tree walk did this
for `*?` and `+?`, but not for the rest:

- `??` under ratchet tried the atom first, so `"ab" ~~ /:r a?? ab/` failed where rakudo matches;
- a separated `+? % sep` under ratchet committed to its minimal count, so
  `"a,a,ab" ~~ /:r a+? % "," b/` matched only `ab`.

Both now follow rakudo. The compiled regex engine (ADR-0135) declined every frugal-and-ratcheted
quantifier (`frugal-ratchet`, the most common decline left in `t/grammar` and `t/regex`). It now
compiles them: the loop keeps its choice point and only the iterations are cut. A separated one whose
atom or separator runs code still declines (`separator-frugal-ratchet-code`), because the walk grows
that chain eagerly and would run the code a different number of times.

Part of [#10255](https://github.com/tokuhirom/mutsu/issues/10255).
