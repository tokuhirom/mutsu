# ADR-0019 D3-2: `CompiledMethodDecl` typed mirror, class walker

`class_body_method_decl`, `role_body_method_decl`, and `augment_class`'s
`MethodDecl` arm each independently re-destructured `Stmt::MethodDecl`'s
19-field AST variant, the same class of drift D2b (`CompiledAttrDecl`) found
and fixed for `Stmt::HasDecl`'s four registration sites.

`CompiledMethodDecl` (`src/opcode.rs`) is now a typed mirror of those 19
fields, built once by `CompiledMethodDecl::from_stmt` — mirroring
`CompiledAttrDecl`'s own shape: `from_stmt` panics on any non-`MethodDecl`
statement, since every call site already matched on one before reaching it.
`params: Vec<String>` is deliberately dropped from the struct: all three
walkers already ignore it (`params: _` at the class/role sites, uncaptured
by augment's `..`), because the parameter names are always recomputed from
`param_defs` — mirroring an unread field would just be extra surface.

This slice converts only `class_body_method_decl` to build one `decl` at its
top and read every field off it instead of the original 19-binding
destructure. A pure mechanical conversion, no behavior change — confirmed
against the full `t/` suite (27810 tests) plus every whitelisted
`S12-methods`, `S14-roles`, `S12-attributes`, `S12-class`, and
`S12-construction` roast file (90 files total), all green.

`role_body_method_decl` and `augment_class`'s `MethodDecl` arm are not yet
migrated (tracked as D3-3/D3-4). This slice deliberately does not fix the
drift ADR-0019's D3 scoping pass found between the three walkers (most
notably `augment_class` missing the `is_lexical_only`/`is_our_only` gating
the class and role walkers both apply, and its duplicate-method detection
not being privacy-aware) — that fix belongs at the point where all three
walkers share one `CompiledMethodDecl::from_stmt` construction site, the way
D2b's unification fixed its own four-way drift by construction rather than
by a targeted patch at each site.
