# `supply { ... }` is wrapped around its emitter where the parser built it

`supply { BODY }` was a typed `ParsedExpr::Supply` node, but the executing
finalization converted it to RakuAST and lowered it back (the second largest
typed family left in the census of the fallback, ADR-15760 slice 2). It now
lowers the body where it stands, in a lexical scope of its own, and hands it to
the same `supply_method_call` that the RakuAST lowering uses; `.AST` and
`EVAL($ast)` are unchanged.
