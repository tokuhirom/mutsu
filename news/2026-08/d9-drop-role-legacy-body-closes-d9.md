# ADR-0019 D9: dropped CompiledRoleDeclPlan::legacy_body, closes D9

`walk_role_body` now iterates the compiler-precomputed `body_plan:
&[RoleBodyOp]` directly instead of a raw, separately-stored `Vec<Stmt>`,
the role-side twin of D6-4's class-body change. `RoleBodyOp` (added in
ADR-0019 D7-4) had sat purely additive with zero non-test consumers, so
this box went straight from "additive typed mirror" to "sole driver, raw
field dropped" in a single PR — the role side needed no separate
default-flip slice the way the class side did.

`RoleBodyOp::Attr` gained a `raw: Box<Stmt>` field (boxed, matching
`Deferred`'s existing boxing rationale for the same enum-size reason) so
`role_body_has_decl`'s existing fallback for a role-level `my`-scoped
attribute — excluded from the compiler's name-keyed `attr_decls` table,
same as the class side — still has a raw statement to read.
`RoleBodyOp::Deferred`'s existing `raw` already covered the walk's other
two raw-statement uses (the `__mutsu_stub_die`/`__mutsu_stub_warn` stub
marker check, and the no-op fallthrough for `SetLine` and every other
deferred-to-composition statement), so no change was needed there.

With every consumer moved onto `body_plan`,
`CompiledRoleDeclPlan::legacy_body`, its one construction site, and
`register_role_decl`'s now-unused `body: &[Stmt]` parameter (replaced by
`body_plan: &[RoleBodyOp]`) are all deleted. This closes ADR-0019's D9 box.

Verified via the full `t/` suite (28,087 tests), all 701 Rust unit tests,
the `S12-class`/`S12-construction`/`S14-roles`/`S05-grammar` roast files
(957 tests, only the pre-existing non-whitelisted `S12-class/open_closed.t`
failure), `scripts/battery-testsuite.sh` (158/164, byte-identical to
baseline), and a hand comparison against `raku` exercising every
`RoleBodyOp` variant in one role composed onto a class (an attribute with
a `my`-scoped sibling to force the fallback path, a nested `does`, a
method, and a nested `my class`) — byte-identical output.

Verification also surfaced a real, pre-existing, unrelated divergence: an
`our`-scoped role attribute (`our $.x` inside a role body) is accepted by
mutsu instead of raising raku's `X::Declaration::OurScopeInRole`. Filed as
`todo/tickets/role-our-scoped-attribute-not-rejected.md` rather than fixed
here, since it is out of scope for a structural field-removal slice.
