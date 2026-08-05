# `start`-containing bodies run compiled; the module-OTF body gate is gone (ADR-0019 C6e-2c)

The last *body* exclusion in the module-single OTF gate is lifted: a routine
whose body contains a `start` block now compiles and dispatches like any other
routine, instead of tree-walking through `call_function_fallback`'s interpreter
arm. This was the largest residual-arm population by far (2,659 of the 3,677
instrumented hits across the roast whitelist, concentrated in recursive-start
subs like `conc-fib`).

The exclusion existed because a *recursive* sub whose start closure captures a
param used to get its capture clobbered under OTF: the recursive call re-bound
the param name in the thread env the closure keeps reading, so after `await`
the captured `$n` held the deepest call's binding (t/start-block-return-value.t
test 3). The ledger anticipated a per-invocation param-isolation design; the
A/B measurement showed it is unnecessary. The compiled caller-env merge
excludes the callee's own params (`routine_writeback_excluded_names`), so each
invocation's binding already stays isolated from the env the closure reads —
the C6d/C6e merge work fixed the substrate out from under the historical
failure.

Verification: an env-gated widened gate ran the full `t/` suite (27,515 tests)
and all whitelisted S17/S07-hyperrace/integration roast files (218 files,
3,004 tests) with zero failures, and gdb confirmed the widened gate really
moves the recursive `conc-fib` call off the interpreter arm (default: thread
recursion hits `eval_block_value_with_pre_post`; widened: the call dispatches
through the OTF compiled entry and never reaches the fallback). A full local
`make roast` on the final change was also green.

Because `start` was the only leaf for which the predicate returned true, the
whole `module_otf_body_needs_interpreter` / `module_otf_stmt_needs_interpreter`
/ `module_otf_expr_needs_interpreter` family is deleted, along with
`RoutineBodyFacts::module_otf_needs_interpreter`.
`def_module_single_sig_body_ok_ignoring_state` is now a pure signature check:
only NativeCall marshalling traits (`is encoded(...)`) keep a def on the
interpreter arm. Body constructs no longer gate compilation at all, which
clears reader class 1 of the `legacy_body` drop
(`todo/deep/c6e-legacy-body-drop-blocked-by-gate-rejected-shapes.md`) and
unblocks C6e-3.

Pinned by `t/start-body-param-compiled.t` (recursive start/await fib, a Str
param read after the recursive await, param reads before/after an unrelated
await, and sibling fan-out isolation — expected values verified against raku).
