# Statement-position calls run the compiled routine body

ADR-0019 C6d-1, final slice. `exec_call` — the statement-position call entry reached via
`ExecCall`/`ExecCallPairs` — carried a full inlined copy of the retired interpreter routine
entry `call_function_def`: its own env save, caller-env push, parameter binding, package
switch, routine/block-stack frames, a `run_block(&def.body)` body run, and a hand-rolled
blanket writeback merge on return. That body run recompiled the routine's AST on every call
(48 hits across the `t/` suite in the C6d survey), and the merge was the legacy
blanket-reconcile rather than the precise merge every expression-position call uses.

The whole block now delegates to `call_routine_def`, the shared compiled entry the other
C6d-1 callers adopted (multi deferral, user operators, reduce/hyper steps, `MAIN`). A
statement call therefore runs the routine's plan-attached bytecode — falling back to one
memoized on-the-fly compile when the plan attached none — and returns through the same
writeback merge, deprecation recording (`cf.deprecated_info`), `empty_sig` rejection,
callsite-line injection, and return-spec finalization as an expression call. The
statement-position pre-dispatch stays where it was: native test-function delegation,
`make`/`made`, the wrap chain, the JSON-module gate, and the proto no-match error.

Two helpers died with the copy: `alias_params_into_current_package` (it existed to mirror
bound parameters under package-qualified names for bodies compiled *without* parameter
locals via `compile_block_raw`; a plan/OTF-compiled routine body bakes its parameters into
local slots, so nothing qualifies them) and the zero-line `check_deprecation_for_def`
wrapper (the compiled entry records deprecation from `cf.deprecated_info`, which both the
plan compiler and the OTF compiler fill from the same `def.deprecated_message`).

This closes C6d-1. The remaining C6d boxes are C6d-2 (grammar token/rule bodies, scoped
against ADR-0009), C6d-3 (two suite-dead sites), and C6d-4 (`call_sub_value`'s code-object
AST path).
