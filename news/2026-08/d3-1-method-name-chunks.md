# ADR-0019 D3-1: method declaration name chunks precompiled

`class_body_method_decl`, `role_body_method_decl`, and `augment_class`'s
`MethodDecl` arm each evaluated a `method ::($name) {...}` computed method
name the same way: `self.eval_block_value(&[Stmt::Expr(expr.clone())])`,
recompiling the expression's bytecode from a raw AST clone on every single
registration. This mirrored the pattern `SubDecl`/`ClassDecl` used before
ADR-0019 C5/D2c precompiled their own computed-name and trait-argument
expressions into child chunks (`CompiledDeclExpr`) at plan-lowering time.

The two walkers with an existing declaration plan — `CompiledClassDeclPlan`
and `CompiledRoleDeclPlan` — now carry a `method_name_chunks:
Vec<Option<CompiledDeclExpr>>`, one entry per top-level `method`/`submethod`
statement in the body (after the same `SyntheticBlock` flattening
`run_class_body`/`walk_role_body` already perform), precompiled once by
`Compiler::compile_method_name_chunks`. Registration reads the chunk at that
statement's position instead of recompiling `name_expr` from a clone.

Position, not name, is the shared key: unlike an attribute's `is
default(...)` chunk (ADR-0019 D2c-1, keyed by the attribute's own unique
name), a method's fallback `name: Symbol` is not reliable to key on — an
indirect declaration with a non-literal expression falls back to a shared
placeholder, and ordinary `multi` methods legitimately share a literal name.
Both sides (the compiler's collector and the two runtime walkers) flatten
`SyntheticBlock` identically and visit `MethodDecl` statements in the same
order with no other filtering, so a cursor threaded through `ClassBodyCx`/
`RoleDeclCx` stays aligned by construction.

`augment_class`'s `MethodDecl` arm keeps evaluating the raw AST expression:
`augment class` is not part of the ADR-0019 declaration-plan system at all
yet (`Stmt::AugmentClass` still indexes `stmt_pool` directly via a legacy
`AugmentClass(u32)` opcode), so there is no compiled plan to attach a chunk
to. Giving it one is separate, larger scope — building the augment
declaration-plan machinery from scratch — left for a later slice.

Extended `t/indirect-declarator-names.t` with a multi-method class and a
role, each interleaving ordinary and `method ::(...)` declarations, to
exercise the positional cursor with more than one indirect name per body
(the prior coverage had exactly one method per declaration, which would not
have caught a misaligned cursor). Verified byte-identical output against
`raku` for the new cases.
