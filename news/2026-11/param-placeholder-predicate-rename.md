# The parameter-placeholder predicate has its own name

`ast::is_placeholder_name` (the execution names a parameter without a written variable runs under)
is renamed `ast::is_param_placeholder_name`, so it is not mistaken for
`ast::placeholders::is_placeholder_name` (the `^a` / `:a` placeholder *variables* of a block).
ADR-15761 now records that moving the test onto `ParamDef.target` needs a `ParamTarget::Literal`
and every site that rewrites a name after the fact (`rakuast::lower`) to set `target` too; that is
the next slice, not this rename.
