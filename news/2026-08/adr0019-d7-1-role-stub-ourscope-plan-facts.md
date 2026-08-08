# ADR-0019 D7-1/D9-1: role stub-ness and our-scope violations precomputed at plan lowering

`CompiledRoleDeclPlan` gains two facts the role plan never had a D1-style equivalent for:

- **`is_stub: bool`** — whether the role body is a yada-stub declaration (`...`, `!!!`, or
  `???`), computed by a new `role_body_is_stub` free function that mirrors
  `Interpreter::role_body_is_stub` exactly, including its looser `.any()` check (any top-level
  statement being a stub call marks the whole role a stub) — unlike the class side's
  `is_stub_routine_body`, which requires the stub to be the body's *only* statement. This is an
  existing, pre-existing divergence between the class and role stub checks; this slice
  preserves it rather than unifying it, per the "precompute, don't change behavior" rule.
- **`our_scope_violation: Option<&'static str>`** — the first our-scoped declaration kind
  (`"class"`, `"subset"`, `"enum"`, `"role"`, `"constant"`, `"variable"`, `"sub"`, or
  `"method"`) found in the role body, if any, computed by a new
  `role_body_our_scope_violation` function mirroring
  `Interpreter::check_role_body_our_scoped_decls`'s scan verbatim.

Both are computed once at plan lowering (`add_role_decl_plan`) instead of `register_role_decl`
re-walking the raw body on every registration. `register_role_decl` now takes both facts as
parameters: `our_scope_violation: Option<&str>` drives the `X::Declaration::OurScopeInRole`
raise directly (the error-construction code moved from the old scan into the registration
function unchanged), and `is_stub_body: bool` replaces the old inline `Self::role_body_is_stub(body)`
call. The two now-dead `Interpreter` methods (`check_role_body_our_scoped_decls`,
`role_body_is_stub`) are deleted — `register_role_decl` was their only caller.

This is D7-1 and D9-1 at once: the ADR names them as the same slice (the role plan's `is_stub`/
our-scope facts are shared infrastructure for both "encode role structure" (D7) and "remove
`CompiledRoleDeclPlan::legacy_body`" (D9)). D7-2..4 and D9-2..5 remain open.

Verified with two new compiler unit tests
(`role_declarations_precompute_stub_and_our_scope_violation`, alongside the existing
`role_declarations_precompute_body_prescan`), the existing role/stub/our-scope `t/` tests, the
full whitelisted `roast/S14-roles/*` suite (24 files, 448 tests), and a full `make test` run.
