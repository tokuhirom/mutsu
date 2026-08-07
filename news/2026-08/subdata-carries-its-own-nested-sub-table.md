# A block's nested subs resolve their compiled bytecode from any calling context

ADR-0019's C6e-3c slice gave `CompiledFunction` and `MethodDef` their own
`compiled_fns` carrier earlier this month, so a routine invoked from a
foreign compilation unit's compiled code could still resolve its own nested
`sub` declarations' bytecode instead of falling back to the AST
interpreter. `SubData` — the runtime value for a bare block or closure, as
opposed to a named routine — was explicitly scoped out of that work, and a
2026-08-07 re-audit found it was a real gap: a `sub` declared inside a block
passed to another module's compiled code (the shape `Test::Util`'s
`group-of` uses) could not resolve its own compiled bytecode and silently
kept interpreting instead.

`CompiledCode` now carries the same `compiled_fns: Option<Arc<CompiledFns>>`
field, populated from the closure's own nested-sub table at compile time
(previously computed and discarded). `SubData` copies it at every
closure-construction site, and the two dispatch fast paths that invoke a
`Sub`'s compiled bytecode — `vm_call_on_value` and `vm_call_map_block` — now
prefer it over the caller's ambient table.

`vm_call_map_block` turned out to have a more general, currently-live
version of the same bug: it substituted a hardcoded empty table for every
`.map`/`.grep` block regardless of context, so a nested named `sub` inside
any such block never resolved its own bytecode — always AST-interpreted,
independent of any C6e-3c timeline. Both call sites are fixed.

Validated with a temporary `MUTSU_FORCE_BODYLESS=1` instrument (forcing
every plan-derived sub to register without an AST body, mimicking a fully
completed `legacy_body` drop): the full `t/` suite (27,755 tests) and the 37
whitelisted roast files exercising `Test::Util`'s `group-of` (3,788 tests)
both pass. Pinned by `t/subdata-nested-sub-compiled-fns.t`.

This closes Class 2 of
`todo/deep/c6e-legacy-body-drop-blocked-by-gate-rejected-shapes.md` (ADR-0019
C6e-3c); the legacy-body field itself is not dropped yet — a fresh
full-suite forced-instrument sweep is still needed to confirm no further
classes remain.
