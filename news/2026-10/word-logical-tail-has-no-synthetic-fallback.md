# The word-logical tail has one shape: `DeclTail`

`my $x = 1 and 2`, `$x = 1 andthen 2`, `my ($a, $b) orelse 3` ... re-attach the loose tail to the
declaration or assignment as `ParsedStmt::DeclTail`. `word_logical_split::tail_stmt` still carried
a fallback for a head `decl_tail::is_head` rejects, a scopeless `SyntheticBlock([head, Expr(tail)])`
that the converter had no arm for. A survey of the whole `t/` suite (debug build, every construction
logged) never reached it, so the fallback is deleted; a head that is not a declaration or assignment
is now a debug assertion.
