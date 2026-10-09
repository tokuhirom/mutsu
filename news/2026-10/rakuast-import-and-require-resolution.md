# RakuAST preserves imported routine and require resolution

The RakuAST frontend now keeps imported routines ahead of same-named IO builtins and mutating listops, including inside a block whose import scope has ended by the time lowering runs. The parser records its user-routine decision on the call; conversion carries it as hidden node metadata and lowering restores it. An imported `put` remains a routine call, and imported `push` and `pop` bypass the compiler's builtin array-method rewrite.

A literal `require Foo` now crosses the frontend boundary as `RakuAST::Statement::Require`. Lowering restores the package-valued target used by the existing compiler and VM, including lexical require stubs, failed loads and expression-position return values. The focused test checks Rakudo's node shape and EVAL behavior; the existing import and require tests pass in frontend mode.

This is an S10 slice of #7564. Hand-built `Statement::Require` and `Statement::Use` constructors remain a separate qualified-name resolution issue (#12446).
