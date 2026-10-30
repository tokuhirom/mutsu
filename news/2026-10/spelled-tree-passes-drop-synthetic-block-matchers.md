# The parser's whole-unit passes no longer match a block the parser never builds

The parser keeps the written declaration nodes (ADR-15334); it has not built a
`SyntheticBlock` since the `has` modifier slice. A census over the spelled tree
of 1500 test files found none, so the arms that matched one in `sink_warn`,
`decl_doc`, `decl_scan`, `stmtlist` (the `unit class Foo:ver` setters) and the
scalar-declaration trailing-comma path were dead and are deleted, as is the
placeholder swap in `outer_redecl`. The two helpers that do read a *lowered*
tree moved to where lowered shapes belong: `decl_target_var_name` to
`ast::lvalue`, `is_destructure_block` (read only by the compiler) to
`ast::signature_decl`. `synthetic-block-sites` 23 -> 10 (what remains is
`type_qualify`, which runs after the lowering, one test over the lowered tree,
and comments).
