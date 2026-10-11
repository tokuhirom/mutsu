# A captured multi dispatcher ranks its candidates out of scope

A `&name` taken from a lexical (non-`our`) `proto` and called after its
declaring scope is gone used to run the first applicable captured candidate
in declaration order. So `M1::g()` returning `&p1` picked `multi p1(@a, @b)`
over the narrower `multi p1(Array $a, Array $b)` for two arrays, where
`raku` picks the latter (#12474).

The captured candidates now become `dispatchee` rows -- the same rows
`&proto.add_dispatchee` registers -- and go through the by-name resolver's
ranking, so narrowness, `where` tie-breaks, `X::Multi::Ambiguous` and
`X::Multi::NoMatch` follow the rules a by-name call follows.
