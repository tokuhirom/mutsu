# The executing path finalizes a unit in place

An executing parse used to carry the unit through `convert` (a `RakuAstNode` tree, with a table of clones standing in for the subtrees the converter could not render) and `lower`. `rakuast::finalize` now lowers each typed node where it stands, takes the spellings off, and converts only what the parser's own tree cannot stand for (a bareword to classify, a regex with code, an operator declaration, a package holding a method of a nested block) and lowers just that. No `RakuAstNode` is built for the rest of the unit, and top-level statements keep the `SetLine` markers the parser wrote.

`.AST`, `EVAL($ast)` and the `MUTSU_RAKUAST=convert` coverage level still read and render the nodes. The precompilation cache format version is bumped (the top-level line markers differ).
