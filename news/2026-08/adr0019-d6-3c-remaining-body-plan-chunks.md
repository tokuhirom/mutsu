# ADR-0019 D6-3c: compile the class-body `CodeAlias`/`ProtoMethod`/`LeavePhaser` chunks

Following D6-3a's `body_plan` skeleton and D6-3b's `Other`/`ClassSub` chunk compile,
this slice compiles the three remaining raw-statement arms — `CodeAlias`
(`our &baz ::= &bar`), `ProtoMethod` (a class-body `proto method`), and
`LeavePhaser` (`will leave { ... }`) — into their own standalone
`CompiledDeclExpr` chunk, the same way `Other`/`ClassSub` already do.
`Compiler::compile_class_body_plan`'s match widened from two arms to all five
raw-statement-carrying arms; the mechanism (`Compiler::compile_decl_stmt_chunk`)
is unchanged.

Each arm still executes its raw statement wholesale at registration today
(`class_body_code_alias`'s trailing `run_block_raw`,
`class_body_proto_method_decl`'s `FunctionDef.body` clone,
`run_class_body_leave_phasers`'s per-phaser `run_block_raw`), so a
single-statement chunk mirrors each exactly — no arm needed a richer typed
payload for this purely-additive slice. (The design document floated reusing
`CompiledProtoDeclPlan`'s shape for `ProtoMethod`; that turned out unnecessary.)

`body_plan` is now a complete, compiled mirror of `legacy_body` with zero
consumers, matching the D6-3 slice plan's own description of this milestone.

Verified via the full `t/` suite (28,023 tests) and the `S12-class`/
`S12-construction`/`S14-roles`/`S05-grammar` (proto/protoregex) roast files
(only the pre-existing, non-whitelisted `S12-class/open_closed.t` failure).

Next: D6-3d, the driver cutover — `run_class_body` switches its statement
source from `legacy_body` to `body_plan`, behind an env-var instrument for
validation before flipping the default.
