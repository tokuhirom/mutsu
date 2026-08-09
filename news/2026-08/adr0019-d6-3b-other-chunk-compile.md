# ADR-0019 D6-3b: compile the class-body `Other` (and `ClassSub`) chunks

Following D6-3a's `body_plan` skeleton, this slice compiles the `Other` arm's raw
statement into its own standalone `CompiledDeclExpr` chunk — the largest and
highest-value reader per the D6/D9 reader inventory
(`todo/deep/adr0019-d6-d9-legacy-body-removal.md`).

`compile_decl_expr_inner`'s child-`Compiler` setup (a standalone unit with no local
slots, so every variable resolves through the declaration's own environment) is
factored out into `Compiler::new_decl_chunk_compiler`, shared by a new
`Compiler::compile_decl_stmt_chunk(&Stmt)` sibling that compiles a whole statement
instead of one `Expr` wrapped in `Stmt::Expr` — the generalization the design
document called for. `ClassSub` gets a chunk through the same mechanism: a top-level
`SubDecl` runs through the identical `class_body_other_stmt` path at registration
(only adding the `class_subs` tail-probe fact on top), matching the original design
sketch's own comment ("the SubDecl tail probe fact + Other chunk").

`token`/`rule` declarations are explicitly excluded — they keep `chunk: None` and
stay on the registration-time `run_block_raw` path, per the phase preamble's
ADR-0009 carve-out. A new compiler unit test
(`class_declarations_body_plan_excludes_token_rule_chunks`) pins this.

Still purely additive: nothing outside the compiler's own unit tests reads a
compiled `Other`/`ClassSub` chunk yet — the driver cutover is D6-3d. Verified via
the full `t/` suite (28,019 tests) and the `S12-class`/`S12-construction`/
`S14-roles` roast files (only the pre-existing, non-whitelisted
`S12-class/open_closed.t` failure, unrelated to this change).

Next: D6-3c (compiling the remaining small arms — `CodeAlias`/`ProtoMethod`/
`LeavePhaser`).
