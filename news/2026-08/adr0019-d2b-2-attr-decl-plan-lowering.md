# ADR-0019 D2b-2: attribute descriptors move to the declaration plan

Class and role declaration plans now carry `attr_decls: Vec<(Symbol,
CompiledAttrDecl)>`, a name-keyed vector of fully-typed attribute descriptors
built once at compile time. This replaces `CompiledClassDeclPlan`'s
class-only `is_default_chunks` field and closes the remaining half of D2b
(D2b-2 in the ADR-0019 checklist): registration no longer calls
`CompiledAttrDecl::from_stmt` on the raw `Stmt::HasDecl` for every plan-backed
`has` declaration it encounters.

The class-side collector needed a real fix, not just a mechanical port. The
previous `is_default_chunks` collector recursed into a `SubDecl`'s body twice
for the same nested `has` statement — once from an explicit loop over the
`SubDecl`'s direct children, once again from a blanket recursive call into
those same children — silently double-pushing an entry for any `has ...  is
default(...)` declared inside a `sub` nested in a class body. It was harmless
under the old scheme (a linear name-keyed scan just finds the first match),
but would have been a real bug for anything order-sensitive. The new
collector instead mirrors `class_own_attribute_names`'s already-proven
traversal shape (used for the D2a own-attribute-name precompute): a
non-recursing second pass that never re-visits a statement it already
processed.

`class_body_has_decl` and `role_body_has_decl` now look their current
statement up in the plan's `attr_decls` by name, falling back to
`CompiledAttrDecl::from_stmt` only on a miss — a class-level `our`/`my`
attribute (deliberately excluded from the collector, matching
`own_attribute_names`'s existing exclusion) or a registration path with no
compiled plan at all (`augment class`, role-pun/mixin synthesis). Role
declaration plans gain this attribute-descriptor lowering for the first
time — roles never had an `is_default_chunks` equivalent — but a role's `is
default(...)` argument deliberately stays `DeclTraitArg::Ast` rather than
being compiled to a chunk, since `role_body_has_decl` only stashes the raw
expression for later composition-time evaluation; that keeps this slice a
pure construction-side move with no behavior change on either plan.

Verified against the full `t/` suite (27,942 tests) and every whitelisted
`S12-attributes`/`S14-roles` roast file (36 files), plus a manual raku-vs-mutsu
comparison covering a nested-sub `is default`, a role instance-attribute
default, and a role `my`-scoped class-level attribute — matching exactly.

This also clears a prerequisite for D6/D9 (removing
`CompiledClassDeclPlan`/`CompiledRoleDeclPlan`'s `legacy_body` field): the
`has`-arm registration read was one of the last readers keeping that field
alive.
