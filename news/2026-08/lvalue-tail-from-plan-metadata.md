# Lvalue routines register body-less; the assign tail comes from plan metadata

The last C6e-3b keep-class with a landable story: a routine-level
`is rw`/`is raw` routine (or one whose tail is an explicit `return-rw`)
kept its AST body because the assignment machinery
(`assign_named_sub_lvalue_with_values`) re-extracted the assign target of
`f() = v` from the body's last expression (`rw_sub_target_expr`).

The plan now records that tail at lowering
(`CompiledRoutineMetadata::rw_tail_expr`, an `Arc<Expr>` present only for
the lvalue shapes), registration seeds it into the new
`FunctionDef::rw_tail_expr`, and the assign path prefers the seeded expr
with the body walk as the metadata-less fallback. A body-less routine code
object reaching the callable-value assign path delegates to the named path
(its installed def carries the tail). With that, the registration
predicate's `is_rw`/`is_raw`/`return-rw` conditions are gone — lvalue
routines register with an empty body like every other safe-class def.

The C6e-3c cut-line is now down to: a plan without resolvable bytecode for
every declared signature (class-walker nested subs), and NativeCall
marshalling traits (measured non-vendorable;
`todo/deep/nativecall-cannot-be-vendored.md`).

Found on the way (pre-existing, verified on v0.20.0): an lvalue sub whose
tail is an array ELEMENT (`sub elem() is rw { @a[1] }`) is not assignable —
`todo/tickets/lvalue-sub-element-tail-not-assignable.md`.

Pinned by `t/lvalue-sub-plan-tail.t` (verified against raku).
