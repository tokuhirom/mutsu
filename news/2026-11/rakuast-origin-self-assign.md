# RakuAST: origins of `$n = $n.abs`

An assignment whose source reads the assigned variable (`$n = $n.abs`,
`$n = $n + 1`) gave the left variable the range of the source's variable, so
the source's own call, the `=` and the left variable lost theirs. The target
is not a term the parser placed, so it no longer takes a range by shape; both
variables, the call and the `Assignment` now have rakudo's ranges (#15780).
The same statement with an `if` modifier still goes another route and is open.
