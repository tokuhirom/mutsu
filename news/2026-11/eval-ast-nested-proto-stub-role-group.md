# EVAL($ast): nested protos, repeated stubs and role groups are not redeclarations

An `EVAL`'d unit (string or RakuAST tree) rejected three shapes that rakudo
accepts. A `proto` in a nested block now shadows the outer family like a
plain `sub` does (the proto registration used its own EVAL exclusion instead
of `eval_blocks_lexical_shadow`); a forward stub `sub f {...}` may repeat
before its definition; and roles of one name are candidates of one role
group, so `role P[$n where ...]` followed by `role P[$n]` is no
redeclaration. The scope-level check in `eval_scope_redecl` now also judges
`proto` declarations (two protos, or a proto and a plain `sub`, of one scope
stay `X::Redeclaration`), and the older duplicate-routine pass in
`check_eval_class_redeclarations` was removed since it was subsumed by it.

Five more `t/` files join the `EVAL(slurp($f).AST)` ratchet. Issue #15869.
