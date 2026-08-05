# Safe-class plan-derived routines register body-less by default (ADR-0019 C6e-3b)

The `MUTSU_DROP_LEGACY_BODY=1` instrument introduced by C6e-3a is now the
default registration behavior, and the env var is gone: a plan-derived
routine whose plan bytecode resolves for every declared signature registers
with an **empty AST body**. Its redeclaration identity, structural
fingerprint, OTF-gate facts and dispatch all run from the plan-recorded
values and the attached `CompiledFunction` — the machinery C6e-3a built and
validated (full `t/`, full `make roast`, and the battery testsuite gate all
green under the instrument in both modes; re-validated after the flip).

The def classes that keep their AST bodies are the C6e-3c cut-line, each
tied to a concrete blocker: a plan whose bytecode keys do not resolve in the
executing table (a nested sub registered from a class-walker method body —
the predicate checks the actual lookup, not the key count), scalar `is
rw`/`is raw` parameters (their wrap-chain relay still lives on the
interpreter carrier —
`todo/tickets/rw-writeback-through-wrap-chain-needs-shared-cells.md`),
lvalue routines (`is rw`/`is raw` at the routine level or a tail
`return-rw`, whose assignment target is extracted from the AST), and
NativeCall marshalling traits.

`CompiledSubDeclPlan::legacy_body` itself still exists — it feeds the
keep-classes and the registration fallback — so the field drop remains
C6e-3c. What this slice changes is that the *common case* (the
overwhelming majority of routine declarations) no longer retains an
executable AST copy on its registered def.

Pinned by `t/legacy-body-drop-instrument.t`, which now exercises the
default behavior (sibling-scope redefinition, code-object map/grep,
wrapping trait_mods, block-lexical escape, `dies-ok &f`, Code sequence
endpoints).
