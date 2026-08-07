# `CompiledSubDeclPlan::legacy_body` is gone — sub declarations carry no AST body

ADR-0019's C6 box set out to make ordinary, multi, `our`, hoisted, exported,
operator, and top-level-method declarations register without an executable
AST body. The blocker was `FunctionDef.body`'s 58 original readers, worked
down group by group across C6a through C6e. `CompiledSubDeclPlan` itself
kept one last field, `legacy_body`, as the final fallback for a handful of
"keep-class" shapes that could not yet resolve their plan-compiled bytecode
at registration time.

Each keep-class was closed in turn: scalar/routine `is rw`/`is raw` params
(bound through shared `ContainerRef` cells instead of the interpreter's
lvalue-extraction machinery), signature alternates (per-slot plan metadata),
class-walker method-nested subs (`MethodDef` gained its own `compiled_fns`
carrier so a nested `sub`'s `RegisterSub` opcode resolves against the
declaring method's table instead of a substituted empty one), the NativeCall
marshalling-trait exclusion (measured to have zero live readers), and the
`vm_call_named_inner.rs` sub-decl-as-last-statement registration fallback.
The final keep-class, closed the same day, was `SubData` (bare
blocks/closures): unlike `CompiledFunction`/`MethodDef`, it carried no table
of its own nested subs, so a `sub` declared inside a block invoked from a
foreign compilation unit's compiled code (`Test::Util`'s `group-of` shape)
silently kept interpreting. `CompiledCode` and `SubData` now both carry a
`compiled_fns` table populated at closure-compile time, and the two dispatch
fast paths that invoke a `Sub`'s compiled bytecode prefer it over the
caller's ambient table.

With every keep-class resolved, a `MUTSU_FORCE_BODYLESS`-gated instrument
that unconditionally emptied every plan-derived body was validated against
the full `t/` suite (27,755 tests) and the *entire* `make roast` whitelist —
both green — confirming no further gap remained. `legacy_body` was then
deleted for real from `CompiledSubDeclPlan`: `exec_register_sub_op`
(`vm_register_sub_ops.rs`) now always registers a plan-derived sub with an
empty body, and the one remaining reader
(`vm_call_named_inner.rs`'s sub-decl-as-last-statement fallback, unreachable
through ordinary syntax) builds a body-less `Sub` value directly. Both
`make test` and `make roast` pass with the field gone.

This closes ADR-0019's C6 box (all of C6a-C6e). `CompiledClassDeclPlan` and
`CompiledRoleDeclPlan` each carry their own, unrelated `legacy_body` field —
Phase D's D6/D9 boxes, not yet started.
