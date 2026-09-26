# RakuAST: an uninitialized `my @a` no longer round-trips as `[(Any)]`

`Q[my @a; @a.push(1); @a].AST.EVAL` returned `[(Any) 1]`, while evaluating the
same source directly (`EVAL q[...]`) gave `[1]`. The L10N::XX test files run
every localized fragment through `.AST("XX").EVAL`, so their "loop, statement
modifier" test failed with an extra `Any` at the front (#9568, found while
fixing #9550).

`lower_var_decl` (`src/rakuast/lower.rs`) turns a `RakuAST::VarDeclaration::Simple`
that has no `initializer` into a declaration whose initial value was always
`Nil`. For a `$` variable that is right. For `@a` it assigned `Nil` into the
array, which became a single `Any` element. The parser gives an uninitialized
declaration a sigil-aware default: an empty Array for `@`, an empty Hash for
`%`. The lowering now does the same.

Pinned by `t/rakuast/rakuast-uninitialized-decl-default.t`.
