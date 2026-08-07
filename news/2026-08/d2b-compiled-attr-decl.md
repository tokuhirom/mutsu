# ADR-0019 D2b (partial): `CompiledAttrDecl` replaces ad-hoc `Stmt::HasDecl` destructuring

Four sites in the interpreter each independently pattern-matched `Stmt::HasDecl`'s
18 fields to build a `ClassAttributeDef`: the class-body walk
(`class_body_has_decl`), the role-body walk (`role_body_has_decl`), the
`augment class`/`augment role` `has` arm, and the compiler's mainline/EVAL
`has`-outside-class case (which built a separate `RuntimeHasDeclSpec` type).
Each destructure ignored a different subset of fields with `_`, so the four
sites drifted out of sync with each other over time — the augment arm, for
example, silently skips `is_default`/`is_type`/`deprecated_message`/
`attribute_built`/class-level (`our`/`my`) attributes that the class-body walk
supports.

`CompiledAttrDecl` (`src/opcode.rs`) is now the one place that destructures
`Stmt::HasDecl`, via `CompiledAttrDecl::from_stmt`. All four sites build one
and read its named fields instead. `RuntimeHasDeclSpec` — previously a
10-field duplicate of the AST shape plus a runtime-only `error: Value` — now
wraps `{ decl: CompiledAttrDecl, error: Value }`, so it no longer maintains a
separate field set at all.

This does not yet move descriptor construction to compile time. The three
registration-time consumers still call `from_stmt` once per `Stmt::HasDecl`
encountered while walking `legacy_body`/`flattened_body`, the same walk they
did before — only the *shape* consumed at each stop changed, not *when* it is
built. Precomputing a `Vec<CompiledAttrDecl>` on `CompiledClassDeclPlan`/
`CompiledRoleDeclPlan` at plan lowering (mirroring D2a's
`own_attribute_names`) is deferred: it requires the precomputed vector's
order to exactly match the registration-time walk's traversal (including
nested-sub-declared attributes and `SyntheticBlock`-flattened list-form `has`
declarations), which is not free to get right and is not required to unblock
D2c/D2d. It becomes unavoidable once D6/D9 drop `legacy_body` outright, since
at that point there is no AST left to call `from_stmt` on.

Verified with `cargo test --lib` (672 tests) and the local attribute/class/
role/augment `prove` surface (26 files, 225 tests), all passing unchanged.
