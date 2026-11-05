# LEAVE blocks run before a variable's `will leave` traits

Inside a block or a sub, `LEAVE { }` blocks and `my $x will leave { }` traits ran in one
interleaved reverse order. Rakudo registers the traits on the scope apart from the blocks, so
every `LEAVE`/`KEEP`/`UNDO` block runs first (last declared first) and the `will` traits after
(last declared first). `Stmt::Phaser` now carries `from_trait`, set by the `will` expansion, and
the scope-exit sections compile through `Compiler::exit_phaser_order`.
