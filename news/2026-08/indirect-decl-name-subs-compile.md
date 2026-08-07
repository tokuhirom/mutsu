# `sub ::($name) {...}` now compiles to bytecode, and a second `legacy_body` keep-class is found

ADR-0019 C6e-3c's remaining step is deleting `CompiledSubDeclPlan::legacy_body`
— the field that lets a plan-derived sub declaration fall back to its
interpreted AST body when its compiled bytecode isn't usable. A prior session
measured this as fully unblocked via an env-gated instrument that forced the
`plan_fully_compiled` half of the registration predicate. Re-running the
equivalent of an actual field deletion — forcing the *whole* body-selection
predicate, so `body` is unconditionally empty regardless of
`plan_fully_compiled`/`primary_compiled` — across the full `t/` suite and full
`make roast` found the field is still load-bearing, for two reasons.

## Runtime-resolved names were never compiled at all

`sub ::($name) (...) {...}` (an indirect/computed declarator name) hit an
explicit early return in the compiler: `if name_expr.is_some() { return; }`,
with the comment "Runtime-resolved sub names cannot be keyed reliably in
compiled_fns." That premise doesn't hold — the compiled-routine lookup key
(`{package}::{name}/{arity}#{fingerprint}`) is a purely internal symbol used
to find the `CompiledFunction` in the plan's own `compiled_fns` table; it has
no relationship to `resolved_name`, the runtime-computed name the routine
actually registers under at `RegisterDecl` time. `name` in the key is just the
parser's placeholder text (the bareword/literal written inside `::(...)`, or
`__INDIRECT_DECL_NAME__` for a non-literal expression) — good enough for
uniqueness, irrelevant to correctness. Removing the early return lets the
body compile like any other sub; `t/indirect-declarator-names.t` test 2 (`sub
::(name) declares a callable sub`), which failed under the forced instrument
before this change, now passes, with zero regressions across the full `t/`
suite in both normal and forced modes.

## A plan-derived def inside a block loses its compiled routine across a foreign call

`roast/S12-subset/subtypes.t` failed under the same forced instrument: `sub
pos-match {...}`, declared inside a block passed as `&tests` to
`Test::Util`'s `group-of`, has `primary_compiled.is_none()` at registration —
*even without forcing* — meaning it already runs interpreted on `main` today.
`group-of` calls `tests()` from within its own compiled code (a different
compilation unit than the test file), and the nested `pos-match` — though
itself a plan-derived def, not a closure — executes its `RegisterSub` opcode
as part of the *block's* bytecode, inheriting whatever `compiled_fns` table
the block's own call path was given. Prior C6e-3c work gave `CompiledFunction`
and `MethodDef` their own `compiled_fns` carrier so a routine's nested subs
resolve correctly across a module boundary (the #5982 fix and its
generalizations), but `SubData` (blocks/closures) was explicitly scoped out
of that work. This repro shows the scoping was too narrow: a def nested
inside a *block*, not just inside a *routine*, needs the same treatment. Not
yet fixed — root cause identified but the carrier work for
block/closure invocation is a fresh, separate slice.

`CompiledSubDeclPlan::legacy_body` stays in place until that second class is
resolved. Findings and the next step are recorded in
`todo/deep/c6e-legacy-body-drop-blocked-by-gate-rejected-shapes.md`.
