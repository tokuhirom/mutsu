# Multi methods rank candidates in rakudo's topological tiers

Multi subs were already ranked in rakudo's topological tiers (#12816, #12823).
Multi methods now are too. The MRO walk, the TypeId-chain resolver and the
mixin dispatch keep the visible multi candidates that did not bind as
*rivals*, and `pick_method_winner` tiers the matches together with them. A
match in a later tier loses to one in an earlier tier. Two
arity-incomparable binders in one tier with no bind-time check set the
`X::Multi::Ambiguous` flag, unless one of them is `is default`. This also
holds across owners: an inherited candidate and a subclass's candidate can be
ambiguous with each other.

The change also fixes a regression from #12816. The tiers are built from
mutsu's ranks, and those read the argument types. A non-binding candidate
whose parameter types are unrelated to the arguments therefore carried an
artificial "unrelated" distance, which could push a binding candidate into a
later tier. For example, `multi w(1, 'em')` put `multi w($f, 'khz')` behind
`multi w(Numeric $n, Str:D $u)` for `w(0.1, 'khz')`. Such a candidate now
takes no part in the tier relation, so rakudo's `khz` is the answer again
for subs and methods alike (#12795).
