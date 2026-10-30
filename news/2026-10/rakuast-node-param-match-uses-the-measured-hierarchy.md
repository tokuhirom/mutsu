# A RakuAST node is matched against a RakuAST parameter type from the measured hierarchy

Checking a `RakuAST::*` node against a `RakuAST::X:D` parameter went through the general type
matcher (~15k instructions per check), although the matcher's last step for such a value is a
lookup in rakudo's measured hierarchy (`rakuast::type_object_does`). `RakuAST::Deparse` has 267
`multi method deparse` candidates, so the first dispatch for each new node class ran that check
once per candidate.

`args_match_param_types_inner` now asks the hierarchy directly for a `ValueView::RakuAst` argument
and a plain or `:D` RakuAST parameter type (`rakuast_node_accepts`). A type object, a `:U`/`:_`
smiley, a parameterization or coercion and any non-RakuAST type take the general path unchanged.
Debug builds recompute every fast answer the general way and assert the two agree, so a case the
shortcut gets wrong fails the `t/rakuast` suite.

Measured with callgrind on six `DEPARSE` snippets per round (profiling build, steady state =
(3 rounds - 1 round) / 2, both binaries built from the same `main`): 2,634M -> 1,602M Ir per round
(-39%) on top of the tiering memo (3,556M before it, -55% together). Wall clock for the six
snippets: 0.9 s -> 0.7 s per round (rakudo 0.065 s). Left: the n x n tiering matrix
(~12% of the run) and the argument matching of the non-RakuAST candidates.
