# Construct RakuAST subroutines

`RakuAST::Sub.new` now constructs parameter-less routine nodes with an optional name and body.
An omitted body receives an empty `Blockoid` and `StatementList`, matching Rakudo's model shape.
The constructor validates its child node types, exposes the routine fields through model
introspection, renders in constructor form, and lowers through the existing compiler under `EVAL`.
