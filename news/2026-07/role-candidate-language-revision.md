# A role candidate's language revision comes from its declaration

A role candidate recorded `crate::parser::current_language_version()` at
*registration* time — the revision ambient when the declaration executed — rather
than the one snapshotted on `Stmt::RoleDecl` when it was parsed, which is how a
class has always done it. For a role declared inside a `use`d module that is the
importer's revision, not the module's.

This only ever produced the right answer by accident: the module export scan used
to leak the module's own `use vX` into the parser global and leave it there, so
the ambient revision happened to equal the declaring module's. Once that leak was
closed (see [sprintf-uppercase-inf](sprintf-uppercase-inf.md)), every candidate of
a role group spread across modules collapsed to a single revision —
`roast/S14-roles/versioning.t` subtest 2 saw `("e", "e", "e")` where the three
candidates come from a 6.c, a 6.d and a 6.e module.

The same reasoning applies independently of that leak, because the ambient
revision is also wrong whenever the module comes out of the **precompilation
cache**: `parse_module_source` returns the cached AST without parsing, so nothing
ever sets the module's revision on the parser global. That made the failure
cache-state dependent — on `main` today the first run after
`rm -rf ~/.cache/mutsu/precomp` passes and every run after it fails, which is why
CI never saw it: its runners always start cold. The parse-time snapshot is
serialized with the AST, so it survives precompilation; the parser global does
not.

`register_role_decl` now takes the parse-time revision as a parameter and stores
it on the `RoleCandidateDef`, mirroring `register_class_decl`. Pinned by
`t/role-candidate-language-revision-from-decl.t`, which loads a 6.c and a 6.e
module declaring candidates of the same role and checks both the candidate list
and the revision each pun reports.
