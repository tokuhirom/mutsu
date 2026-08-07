# ADR-0019 D2a: attribute pre-scan facts precomputed as plan data

D2 asked to encode attributes and generated accessors as typed plan
operations. A survey done before starting found the box is not D1-shaped:
D1 discovered that most class structural data (parents, repr, visibility,
lexical/package aliases) was already typed-plan-driven from Phase A3/A4, but
attributes had **zero** existing plan coverage. `CompiledClassDeclPlan` and
`CompiledRoleDeclPlan` carried no attribute fields at all; registration
walked `Stmt::HasDecl` at four independent sites; generated accessors were
resolved by a special-cased runtime lookup (`class_introspection.rs`) rather
than `MethodEntry` rows; and attribute defaults were evaluated by raw
`eval_block_value` at six call sites, with `CompiledDeclExpr` (the C5
re-entrant-bytecode mechanism) not involved anywhere.

Given that shape, D2 is subdivided the way C6 and D0 were once their real
size became clear: measure, then split. D2a takes the one slice that *is*
D1-shaped — two runtime pre-scans that re-derive pure syntactic facts from
the body on every registration, contributing no new attribute semantics of
their own:

- `run_class_body`'s pre-scan built the set of attribute names valid for
  `$!attr` access: flattened top-level `has` declarations (after expanding
  `has ($a, $b)` list-form `SyntheticBlock`s) plus any `has` nested inside a
  `sub` in the class body (`class C { sub f { has $.x } }`, which Rakudo
  still registers on the class). This set now lives in
  `CompiledClassDeclPlan::own_attribute_names`, computed once at plan
  lowering by `class_own_attribute_names`/`collect_nested_has_decl_names`
  (mirroring the runtime's own `collect_nested_class_has_decls`), threaded
  through `ClassDeclModifiers`.
- `walk_role_body`'s pre-scan combined three unrelated facts in one loop:
  attribute names the role declares, module names it `use`s/`need`s/
  `import`s (needed because a role's method-signature validation runs before
  the body's own `use` has loaded), and types the body declares itself (`my
  enum`, `my class`, ...). All three move to
  `CompiledRoleDeclPlan::{own_attribute_names,body_used_modules,body_declared_types}`,
  computed once by `role_body_prescan` and threaded as three new
  `register_role_decl` parameters.

Registration still walks `legacy_body` for the actual `has`-arm dispatch —
typing full attribute descriptors (a `CompiledAttrDecl` covering the whole
`Stmt::HasDecl` surface, subsuming the existing `RuntimeHasDeclSpec`) is
D2b, not this slice. This PR changes no attribute semantics: no new
fallback, and the two callers that build a `ClassDeclModifiers` without a
compiled plan (role-pun registration, runtime mixin-class synthesis) both
already pass an empty body, so they simply supply `own_attribute_names: &[]`
— the same pattern D1 used for `is_stub`/`trusts`.

Pinned by two new compiler unit tests
(`class_declarations_precompute_own_attribute_names`,
`role_declarations_precompute_body_prescan`) alongside the existing
attribute/class/role test surface. Full `t/` (27,765 tests) passes
unchanged; the roast attribute/class/role surface is unchanged (two
pre-existing non-whitelisted failures, `S12-attributes/trusts.t` and
`S12-class/open_closed.t`, reproduce identically on `main` and are unrelated
to this change).
