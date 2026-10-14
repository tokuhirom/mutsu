# `is default` on a role-typed operator multi wins the tie against the core candidate

`multi infix:<+>(U $a, U $b) is default` (CSS::Units' unit-carrying arithmetic) was ignored for operands that are an
Int with the role mixed in (`1pt + 2pt`): the user candidate and the core `(Int:D, Int:D)` are equally narrow by
nominal type, and mutsu broke the tie on type distance in the core's favour. Rakudo reports such a tie as ambiguous
unless a candidate is `is default`, which then wins; `core_infix_candidate_wins` now says so. CSS::Properties' other
blockers are tracked in #13280.
