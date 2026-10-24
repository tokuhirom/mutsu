# RakuAST Stage 2: sigilless declarations are parser nodes

`my \x = 5`, `my Int \x := $s` and `my \x .= new` are kept as `ParsedStmt::SigillessDecl`
(the declaration as written, including `my \x = my $y = 3`, whose initializer is now the
inner declaration itself). Lowering builds the compiler's bookkeeping with
`build_sigilless_bind_stmt` / `sigilless_decl::call_assign_expansion`, with the parse-scope
or the unit's declared terms deciding value terms. `sigilless_decl::declaration`, its
expansion reader, the anonymous-`$` rename (`is_anon_sigilless_source`) and the
`loop_cond_decl::recognize` reader (dead since `while`/`until` became a node) are deleted.
Tracked in #14940.
