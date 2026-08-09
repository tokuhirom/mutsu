# ADR-0019 D8-4: drop `RoleDef::deferred_body_stmts`, closing D8

`RoleDef::deferred_body_stmts` — the raw `Vec<Stmt>` that `walk_role_body` mirrored every
non-declaration role-body statement into at registration time — has been dropped outright. It had
been write-only since D8-2 landed: every composition site (pun, `does`, runtime mixin, parametric
composition) already runs the precompiled `deferred_body` ops instead, and nothing else ever read
the raw vec.

The `walk_role_body` catch-all arm that used to push onto it becomes a no-op (the compiler's
`deferred_body_ops` already covers running that statement at every composition site), and
`RoleDeclCx::is_parametric` is dropped too — that push, identically written in both its
`is_parametric` and non-parametric branches (a pre-existing redundant split), was its only reader.

Pure dead-field/dead-branch removal with no behavior change, confirmed by grepping for every
remaining reference before deleting it. This closes ADR-0019's D8 box ("compile role
declaration-time bodies and traits") now that D8-1 through D8-4 have all landed.
