# A routine calling a block-imported function is no longer compiled to TRIR

`{ use M; sub f($s) { imported($s) ?? 1 !! 2 } }` died with `Unknown function:
imported` once `f` ran. The typed IR compiles a routine from its AST and links
its calls by name, so it never consulted the block's lexical import set
(`LexImportCall`) that binds `imported`. A routine whose calls resolve through
an import set now declines TRIR and runs on bytecode (#14140).
