# A bare block that only reads `$_` sees writes to its argument variable

Raku binds a bare block's implicit `$_` raw to its argument, so
`my $x = 1; { $x = 5; .say }($x)` prints `5`. mutsu aliased the topic only for
a block that writes `$_` itself, and a block that only read `$_` saw a copy
taken at the call. The read case printed `1`, and so did a dynamic argument
(`block $*V`) that the block or one of its callees reassigned. Functional::LinkedList's
`mutate(&block)` relies on that dynamic case.

The topic now reuses the caller's variable cell when the variable already has
one. When it has none, the topic boxes the variable in two cases: the block
writes the variable by name, or the variable is dynamic and the block makes
calls. A block that touches neither `$_` nor the variable still takes no
alias and registers no writeback (#11298).
