# Plain code variables are carried through the executing finalization

`&f`, `&foo-bar` and the like were converted to RakuAST and lowered back by
`finalize_unit` on the assumption that the unit's routines classify them. The
conversion only reads them as a lexical `&name` and the lowering hands the same
name back, which `finalization_is_the_ordinary_tree` now pins. A plain
identifier is carried as the parser built it; a package-qualified or operator
name (`&infix:<+>`) is still converted (ADR-15760 slice 2). `.AST` and
`EVAL($ast)` are unchanged.
