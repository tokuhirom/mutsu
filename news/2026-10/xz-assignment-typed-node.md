# `X=` and `Z=` are a typed parser node

The cross and zip metaoperator assignments (`@a X= 1`, `@a Z= @b`, `my @a Z= 1, 2`) were built as
private expansions (`MetaOp` over `X`/`=`, a `__mutsu_zip_assign` call) that `.AST` undid with
two reverse recognizers. They are now `CompoundAssign { meta: BracketedMeta(Cross|Zip, depth), op:
"=" }`, which `.AST` renders as rakudo's `ApplyListInfix` over `MetaInfix::Cross|Zip(Assignment(:item))`
and `lower` reads back into the same node. `rakuast/cross_assign.rs` and `rakuast/zip_assign.rs` are
deleted. A `Z=` in expression position now assigns element-wise, as the statement form already did.
