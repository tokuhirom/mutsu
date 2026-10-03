# RakuAST: `pi`/`e`/`tau` are `Term::Name`, `now` is `Term::Named`

The `.AST` converter now renders the math constants as `RakuAST::Term::Name`
(keeping the `π`/`τ`/`𝑒` spelling at statement level) and the argument-less
`now` term as the new `RakuAST::Term::Named.new("now")`. In the write
direction `Term::Name` lowers these identifiers to their numeric value and
`Term::Named "now"` to the `now` call, so `EVAL` of each node gives the term's
value (#11333).
