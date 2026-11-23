# RakuAST::Node.dump

`RakuAST::Node.dump` (and its parts `dump-markers`, `dump-origin`,
`dump-extras`, `dump-children`) now prints a tree the way rakudo 2026.11 does:
one line per node with its class, its markers (`▪` a block statement, `𝄞` a
statement-level origin, a class's own `【+】` / `【say】` / `【$x】` / `🔆`),
`file:line` for a key origin and the escaped source text it covers, a
`QuotedString`'s postprocessors, then its children. `Origin.is-key` and
`Origin::Source.location-of-pos` exist too, and a parameter's target now visits
a `VarDeclaration::Simple` that knows its name. The sink marker `⚓` and the
thunk lines wait on sink propagation (#19106) and thunks (#19107) (#18333).
