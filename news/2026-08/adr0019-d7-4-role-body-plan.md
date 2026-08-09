# ADR-0019 D7-4: role body compiled into a typed op walk, closing D7

`CompiledRoleDeclPlan` gained `body_plan: Vec<RoleBodyOp>`, a new typed enum
(`Attr`/`Method`/`Parent`/`Deferred`) computed at plan lowering by a new
`role_body_plan` free function that mirrors `walk_role_body`'s own dispatch
loop: a single-level `SyntheticBlock` flatten (a role body has no nested-sub
`has` collection, unlike the class side), classified the same way the runtime
match does.

This is the role-side twin of D6-3a's class `body_plan`, but deliberately
narrower: a role body has no `ClassSub`/`CodeAlias`/`ProtoMethod`/
`LeavePhaser` arms, and its `Deferred` catch-all (which also absorbs the
`__mutsu_stub_die`/`__mutsu_stub_warn` stub marker call and `SetLine`
markers) carries no compiled chunk — deferred-statement chunk compilation is
D8's job, on a separate `RoleDef::deferred_body` type. `Deferred`'s raw
statement is boxed (`Box<Stmt>`): unlike `ClassBodyOp`, whose every non-tiny
variant also carries a same-size `Stmt`, `RoleBodyOp`'s `Attr`/`Method`/
`Parent` variants are all marker-sized, so an unboxed `Stmt` tripped
`clippy::large_enum_variant`.

Purely additive — no non-test consumer reads the field yet — and pinned by a
new compiler unit test (`role_declarations_precompute_body_plan`) asserting
`body_plan.len()` against an independently re-derived flattened-statement
count and the typed op sequence, including the role-header `does` clause's
synthetic `DoesDecl` (prepended to the body ahead of any body-level `does`,
both classifying as `Parent`). Verified via the full `t/` suite (28,037
tests).

With D7-1 (=D9-1), D7-2 (=D2b-2's role half), D7-3, and now D7-4 all landed,
ADR-0019's D7 box ("Encode role structure and composition") is complete.
