# A call of a `rule` or `token` hands its caller one match

A `<name>` call of a `rule` or `token` committed only when the call site was
itself ratcheted. Called from a `regex`, the callee's internal choice points
survived its return, so when the caller later failed it re-entered the callee
and matched it again at the same position, running the grammar actions once per
alternative. That mattered whenever the callee held a term rakudo leaves
backtrackable inside the rule (a quantified term followed by significant
whitespace): `rule property { <.ws>? <Ident> ':' }` made CSS::Grammar parse a
declaration's value twice, and its "unknown units" warnings came out doubled.

The compiled program now knows it is the body of a `rule`/`token` (the body was
parsed from `:ratchet` source) and a frame entered for it commits when it
returns, as rakudo's ratcheted declarators do. CSS::Grammar's six test files
now match rakudo (87, 125, 118, 32, 8 and 123 assertions).
