# `PRE { ... }.blorst` is rakudo's `X::Phaser::PrePost ... unless` wrapper

`RakuAST::Deparse` reads a `PRE` phaser's condition back through
`$ast.blorst.condition-modifier.expression`: rakudo wraps the written statement into
`X::Phaser::PrePost.new(condition => "SRC").throw unless COND`. mutsu's `Phaser::Pre.blorst`
had no `condition-modifier`, so any file containing a `PRE` phaser aborted `.DEPARSE`.
`blorst` is now that wrapper (built from the verbatim condition source the node already
records), so `PRE { ... }` and the bare `PRE COND` deparse as in rakudo (#14845).
