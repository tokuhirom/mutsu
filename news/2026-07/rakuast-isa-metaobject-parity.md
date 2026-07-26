# RakuAST `.isa` and `.^isa` share the registered hierarchy

Registered `RakuAST::*` type objects already participated in the namespace and
semantic hierarchy under smartmatch, but the two direct introspection forms
still used the generic class registry. Consequently,
`RakuAST::IntLiteral ~~ RakuAST::Term` was true while
`RakuAST::IntLiteral.isa(RakuAST::Term)` and
`RakuAST::IntLiteral.^isa(RakuAST::Term)` were false. Metaobject `isa` also
rejected concrete RakuAST node values because it only recognized package and
ordinary instance representations.

The ordinary type-object dispatch and ClassHOW dispatch now delegate RakuAST
queries to the model layer's single hierarchy predicate. ClassHOW also derives
the concrete type from a `Value::RakuAst` node, making all three introspection
forms agree without registering duplicate runtime classes.

Pinned by the expanded `t/rakuast-type-objects.t`.
