# ADR-0019 C7: forward-declared top-level subs no longer compile on demand

C7 asked for the sub-registration AST adapter to go: delete dead sub-shaped
walker branches and prove the routine registry never compiles a migrated
declaration on demand. The one live adapter turned out to be
`preregister_top_level_subs`, the pass that lets a call appearing between a
forward declaration and its real body (`sub add($a,$b); is add(1,2),3; sub
add($a,$b) {...}`) resolve to the real body instead of the empty stub. It ran
before the mainline was compiled, so it built its temporary `FunctionDef`
straight from the raw AST body with `compiled: None` — leaving the first
call in that window to compile the body on the fly via
`otf_compile_function_def`, and leaving a real (if narrow) instance of
exactly the AST-registration path ADR-0019 is retiring.

The fix keeps the pass's AST-based *identification* logic (which top-level
subs form a stub-then-full pair) but installs through the same
`register_compiled_sub_decl` entry point the plan-compiled path already
uses, with a new `compile_forward_declared_sub` helper that calls
`otf_compile_function_def` once, eagerly, at registration time instead of
leaving it for the first call. The routine's `CompiledRoutineMetadata` comes
from `crate::opcode::compiled_routine_metadata` (now `pub(crate)`, previously
private to plan lowering), so the installed `FunctionDef` carries the same
derived facts a plan-lowered declaration would.

This let three now-dead functions go: `register_sub_decl`,
`register_sub_decl_fp`, and `register_sub_decl_as_global` had exactly one
live caller between them — this same preregistration pass — once its
`is export` branch was rewritten to reuse `register_compiled_sub_decl` under
a temporarily-swapped `GLOBAL` package instead of duplicating the
`FunctionDef` construction by hand.

Pinned by a new unit test (`forward_declared_sub_installs_with_compiled_bytecode`
in `src/runtime/run_prelude.rs`) asserting the registered def's `compiled` is
`Some` immediately after preregistration, plus the existing
`t/forward-declaration.t`. Full `t/` (27,761 tests) and the roast whitelist
pass unchanged.
