# The assignment writebacks, the hyper prefix and the atomic variable access have one lowering each

ADR-15334 slice 1. The parser used to spell `__mutsu_assign_{method,named_sub,callable}_lvalue`,
`__mutsu_hyper_prefix` and `__mutsu_atomic_{fetch,store}_var` itself, at a dozen sites, and the
assignment target dispatch existed twice (the parser's `assign_to_target_expr` and
`rakuast::assign_lower::lower_expression`).

The writeback constructors and the one target dispatch now live in `rakuast::assign_writeback` and
`rakuast::assign_target`; every parser site that read an `=` calls them, and `lower_expression` falls
back to the dispatch instead of keeping a second copy of its arms. `-« @a`, `++<< @a`, `⚛$x` and
`$x ⚛= v` are the typed `ParsedExpr::HyperPrefix` and `ParsedExpr::AtomicVar` nodes: the parser
builds them, `.AST` renders them from the node, and `convert` no longer recognizes either spelling by
name. `temp $obj.attr = v` reads its writeback apart through the same module.

`t/rakuast/rakuast-stage2-assign-lvalue-paths.t` runs the family as a file, as an `EVAL` string and as
`EVAL` of the string's `.AST`, with the names declared in the calling scope and inside the unit, and
passes on rakudo too. It found one pre-existing divergence, filed as #15388.
