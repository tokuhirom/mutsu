# RakuAST keeps the `.=` source form of an expression-position declaration

`Q|(my Int $x .= new)|.AST` now renders `Initializer::CallAssign` like Rakudo,
instead of an `Initializer::Assign` over the expanded `ApplyPostfix` call.
A declaration outside the statement-level source-form record keeps its `.=`
spelling in the transparent dotty marker (`Expr::CompoundAssign`, op `.=`), so
the compiler still sees the expanded call and the lexical registration and
writable lvalue of `(my Int $x .= new).=new(42)` are unchanged (#11431).
