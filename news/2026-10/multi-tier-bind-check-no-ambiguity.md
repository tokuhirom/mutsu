# A multi tier holding a literal or `where` candidate is trial-bound

Rakudo resolves a tier of unordered multi candidates that holds a candidate with
a literal or a `where` by trial binding in declaration order, so a tie inside it
is not `X::Multi::Ambiguous` (CSS::Module's `resolved-type(Int, Str, Int)`).
`assign_tiers` now records whether a tier has such a candidate that can take the
call, and the tie settlement honours it. A candidate that cannot take the
arguments, by type or arity, still does not count.
