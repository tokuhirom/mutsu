# RakuAST keeps empty statements

A `;` that terminates no statement (`1;; 2`, a leading `;`, a trailing `;;`) is now a
`RakuAST::Statement::Empty` in `.AST`, with the zero-width origin rakudo gives it
(at that `;`), so `visit-children` walkers see the same statement list as on rakudo.
The parser tags these only for a `.AST` parse (`Spelling::EmptyStatement`) using
the separator rule `use trace` already follows (a block statement's own `;` is
its terminator); EVAL drops them again, so a trailing empty statement is still not
a block's value.

Part of #14820 (first row).
