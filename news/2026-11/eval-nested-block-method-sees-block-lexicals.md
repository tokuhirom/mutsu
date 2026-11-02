# A nested-block method keeps its block's lexicals inside EVAL

`class C { { my $n = 7; method m { $n } } }` died with "Variable '$n' is not
declared" when it was run through `EVAL` (or `EVAL` of its `.AST`), though the
same source ran as a file. The method is hoisted to the package body, where the
EVAL undeclared-variable scan no longer saw the block's `my $n`. The scan now
skips a hoisted method; the block's `NestedMethodCapture` marker carries the
same body and is checked where `$n` is declared. The path-parity test covers
the bare block, `if`, shared-lexical, role and grammar rows on all three paths.
