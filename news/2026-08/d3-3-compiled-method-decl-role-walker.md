# ADR-0019 D3-3: `CompiledMethodDecl` for the role walker

Continuing D3-2 (the class-body `method` arm's conversion to the shared
`CompiledMethodDecl` typed mirror), `role_body_method_decl` now also builds
one `decl = CompiledMethodDecl::from_stmt(stmt)` at its top and reads every
field off it instead of the original 19-binding `Stmt::MethodDecl`
destructure — the same pure mechanical conversion, no behavior change.

This walk never read `is_our`, `our_variable_form`, `custom_traits`,
`is_export`, or `export_tags` (a role method is never `our`-registered as a
package sub, and custom traits/exports on a role method go unhandled at this
site) — that gap is unchanged by this slice, now expressed as unread
`CompiledMethodDecl` fields rather than `_`-ignored destructure bindings.

Verified the same way as D3-2: the full `t/` suite (27810 tests) plus every
whitelisted `S12-methods`, `S14-roles`, `S12-attributes`, `S12-class`, and
`S12-construction` roast file (90 files), all green.

`augment_class`'s `MethodDecl` arm (D3-4) is the last of the three walkers.
Only once it also builds from `CompiledMethodDecl::from_stmt` does the drift
ADR-0019's D3 scoping pass found between all three — most notably
`augment_class` missing the class/role walkers' `is_lexical_only`/
`is_our_only` gating and privacy-aware duplicate detection — become fixable
at one shared construction site, the way D2b's `CompiledAttrDecl` unification
fixed its own four-way drift by construction.
