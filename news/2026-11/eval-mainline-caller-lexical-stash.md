An AST EVAL mainline no longer reports its own subroutines through `CALLER::LEXICAL::`. A calling routine's lexical stash still lists its file-scope, package, and block-scoped routines.
