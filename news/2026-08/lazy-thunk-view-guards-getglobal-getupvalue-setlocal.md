# Guard the remaining unguarded LazyThunk `.view()` sites

`OpCode::GetGlobal`, `exec_get_upvalue_op` (`OpCode::GetUpvalue`), and the `SetLocal`
readonly-marking check each called `Value::view()` unconditionally to check "is this a
`LazyThunk`" before forcing it — and `view()` fully materializes a lazily-represented
`Match` value as a side effect, so a Match read via any of these three ops paid an
unnecessary full materialization even though it was never a thunk.

`exec_get_local_op` (`OpCode::GetLocal`) already guarded the same check behind a cheap
`is_lazy_thunk_value()` tag probe first, with a comment noting the exact reason. The
other three sites were identified as the same class of bug during
`todo/tickets/yaml-parse-throughput.md`'s round 7 investigation but left unfixed pending
a higher-value dominant-call-site fix (round 8). This closes that follow-up: all three
now probe `is_lazy_thunk_value()` before calling `.view()`, mirroring `exec_get_local_op`.

The fix is a pure decision-preserving change (the answer to "is this a LazyThunk" is
unchanged, only whether reaching it forces a `view()`), confirmed by the full `t/` suite
(3168 files, 29458 tests) passing with no output differences.
