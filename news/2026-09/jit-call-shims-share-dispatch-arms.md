# JIT call shims share their dispatch arms' bodies

The Tier A JIT shims for `CallFunc`/`CallFuncNamed`, `CallMethod`,
`CallMethodMut`, `Return`, `JumpIfFalse`/`JumpIfTrue`/`JumpIfNotNil` and
`StateVarInitGuard` were hand-kept copies of their `exec_one_dispatch` arms,
and the copies had drifted. None of the call shims ran the `use fatal`
argument check, so a Failure argument exploded while the calling chunk was
interpreted and passed straight through once the chunk went native. The
`Return` shim also dropped the EVAL-context return target (ADR-0037 Slice 4).

Each opcode's whole per-site behavior now lives in one function in
`src/vm/vm_call_site_ops.rs` (`exec_call_func_site`, `exec_call_method_site`,
`exec_call_method_mut_site`, `exec_return_site` and the `*_taken` jump
conditions). The dispatch arm only advances `ip`, and the shim is a thin
`extern "C"` adapter that maps the result to a status, so the two cannot
diverge again. `t/vm/codegen/jit-call-shims-on-off-parity.t` compares
JIT-on and JIT-off output for `use fatal` arguments and a call-heavy program
(#9452).
