# A `multi` declared after `&proto.wrap(...)` joins the wrapped dispatch

`&f.wrap(...)` followed by a later `multi sub f(Num $x)` died with "Cannot resolve caller f(Num:D)"
though the candidate list printed it (#15925): the wrapper's original is the dispatcher value `&f`
captured when `.wrap` ran, and it only dispatched over the candidates it had captured. Rakudo sees every
`multi` of the scope at compile time. A captured dispatcher whose candidates are all still live
members of the family now dispatches over the family as it is (`captured_candidates_are_live_subset`),
so a candidate declared afterwards is reached, for a wrapped `proto` and for `my &g = &f`.
Pinned by `t/routines/dispatch/multi-declared-after-proto-wrap.t`.
