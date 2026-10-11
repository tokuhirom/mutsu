# A slurpy multi candidate competes with an optional-positional one

Multi-sub dispatch tries its candidates in stages, by registered arity. A
candidate with an optional positional parameter and the call's arity
(`multi s(Any $x, Any $y?)`) used to be ranked and bound before any slurpy
candidate of another arity was looked at. So `s(1, 2)` answered `two-opt`
where rakudo answers `slurpy` for `multi s(Int $x, *@r)`: when the two
candidates' minimum arities agree, rakudo compares them on their shared
positions, and `Int` is narrower than `Any`.

Slurpy candidates now join that stage, and `MultiRank` decides between them.
Since #12776 it carries rakudo's arity-mismatch rule, so `multi s(Any $x,
Any $y)` still beats the slurpy candidate, because both its arities differ.
The package-qualified (`M::s(1, 2)`) and proto (`{*}`) paths rank the same
way, through one shared `merge_flexible_arity_candidates` (#12791).
