# A `<name>` call over protoless `multi token` candidates is a multi dispatch

A grammar rule declared as several `multi token` / `multi rule` / `multi regex`
candidates with no `proto` (and no `:sym<>` variants) used to be matched as a
longest-token alternation: `<t>` ran every candidate and unioned the ends, so
`multi token t { a }; multi token t { b }` parsed `b` where rakudo dies with
`Ambiguous call to 't(G: )'`.

Without a proto, rakudo treats the call as an ordinary multi-method dispatch
over the candidates' signatures, and so does mutsu now
([#11875](https://github.com/tokuhirom/mutsu/issues/11875)):

- the narrowest signature wins and is the only candidate that runs (a literal
  or `Int` parameter beats a plain `$x`, a zero-argument candidate answers
  `<t>`, a one-argument candidate answers `<t(1)>`);
- two equally narrow candidates raise `X::Multi::Ambiguous`;
- a call no candidate's signature accepts raises `X::Multi::NoMatch`.

The dispatch reuses the one multi-candidate selection that subs and methods
share (`choose_best_matching_candidate`). The error is raised only by the call
that actually runs: the prefilter, call-graph and LTM analyses that read the
same candidate list see every candidate, so a branch the match never reaches
(`token TOP { x | <t> }` on `x`) does not die, and a failed dispatch is never
memoized into a quiet non-match, so the next parse dies again.

A proto's `:sym<>` candidates keep their longest-token ranking; only the
protoless call changed. Pinned by `t/grammar/multi-token-protoless-dispatch.t`,
whose expectations were taken from `raku`.
