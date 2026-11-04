# RakuAST: the `=config` Pod directive

`=config head1 :numbered` was not converted, so it was missing from `.AST`. It
is now a `Doc::Block` of type `config` with `directive => True`, the pairs as
its config and the configured block type as its only paragraph, with the same
ranges rakudo gives (#14880). `table` blocks (`Doc::LegacyRow`) are still open.
