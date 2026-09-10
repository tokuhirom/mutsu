use v6;
use Test;
use lib 't/lib';
use NontrivialProtoBodyNestedSub;

# A non-trivial proto body (`proto foo($x) { ...; {*} }`) that declares its
# own nested `my sub` compiles via `vm_try_run_nontrivial_proto_body`, which
# ran the proto body's compiled bytecode against the *caller's*
# `compiled_fns` table instead of the proto body's own
# (`CompiledFunction::compiled_fns`, ADR-0019 C6e-3c). For a module-imported
# proto, those two tables diverge, so the nested sub's `RegisterDecl` opcode
# could not resolve its own compiled key and silently fell back to
# tree-walking its AST body instead of running compiled bytecode. Behavior
# was already correct via that fallback; this pins the behavior post-fix
# (mirrors t/nested-sub-in-method-compiled.t and the `call_shared_state_body`
# fix in vm_call_func_ops.rs).

plan 2;

is labeled(5), 'helper(5)', 'nested sub inside a non-trivial imported proto body (Int candidate)';
is labeled("x"), 'helper(x)', 'nested sub inside a non-trivial imported proto body (Str candidate)';
