# A closure argument to a custom infix operator now writes back to the caller

A closure passed as an argument to a custom user-defined infix operator
(`sub infix:<times>(Int $num, &closure) {...}`, called as
`20 times { $value++ }`) did not write its mutation of an outer lexical back
to the caller's scope once the statement finished — even though the mutation
was visible while still inside the callee's dynamic extent. The same closure
passed to an ordinary named sub call (`my_times(20, { $value++ })`) already
worked correctly, so the value only went missing through the custom-infix
call path.

Root cause: `exec_infix_func_op` (the `OpCode::InfixFunc` VM handler,
`src/vm/vm_flipflop_ops.rs`) drives the same underlying call machinery as an
ordinary function call (`try_user_infix` / `compile_and_call_function_def` →
`call_compiled_function_named`), which stages any `rw`-writeback source into
`pending_rw_writeback_sources` on the way out. Every other call-opcode
handler (`OpCode::CallFunc`, `OpCode::ExecCall`) drains that staged set into
the caller's own local slot via `apply_pending_rw_writeback(code)` right
after the call — `exec_infix_func_op` never did, so the caller kept reading
its stale, never-refreshed local slot for `$value`.

Fixed by adding the same `apply_pending_rw_writeback(code)` call to
`exec_infix_func_op`, covering all three of its call-out branches (the
lexical `&infix:<op>` shadow early return, `try_user_infix`, and the
list-associative `compile_and_call_function_def` fallback).

Found via the `PSpec` distribution's own test suite
(`todo/tickets/dist-test-suite-failures-batch.md`): its `times` helper
(`sub infix:<times>(Int $num, &closure) { for ^$num { closure() } }`) is
exactly this shape. Pinned by `t/user-infix-closure-arg-writeback.t`.
