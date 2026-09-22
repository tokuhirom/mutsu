# `floor()`/`ceiling()` free functions now distinguish native `num` from boxed `Num`

[#9012](https://github.com/tokuhirom/mutsu/issues/9012) reported that mutsu's `floor()`/`ceiling()`
**free functions** always returned `Num` for a `Num`-shaped argument, matching rakudo only for a
genuinely native `num` scalar and getting the far more common boxed case wrong: `ceiling(3.2e0)` is
`(Int)` in rakudo (a literal `Num`, boxed), and so is `ceiling(1 + $n / sqrt(2))` (a computed `Num`).
Only `ceiling(my num $y = 3.2e0)` — a genuinely native container — stays `Num`. This surfaced via the
`Graph` ecosystem distribution's `Graph::MinCuttish.find-minimum-cut(method => 'karger-stein')`, whose
`UInt:D :$th` parameter is computed as `ceiling(1 + $n / sqrt(2))`: mutsu's boxed-`Num` result failed
the `UInt:D` type check with a misleading "No matching candidates for method: karger-contract" (the
real failure — a type-check on the sole `!karger-contract` private method — was misreported).

The obvious one-line fix (always return `Int` from the free-function arms) was tried first and
reverted: it broke `roast/S02-types/num.t`'s `'ceiling(num)'` subtest, which pins the native-`num`
case staying `Num`. mutsu has no `Value`-level tag distinguishing a native `num` container from a
boxed `Num` — both are the same `ValueView::Num(f64)` — so the fix has to come from the call site's
static argument shape, not from the runtime value.

`Compiler::literal_native_args_mask` already tracks a related but different per-call-site fact:
whether an argument was written as a literal, published through `OpCode::CallFunc`'s
`literal_native_args` bitmask and read by multi-dispatch candidate ranking
(`unwrap_varref_for_dispatch_at`) to rank `d(5)` as `int` the way rakudo does. That reading disagrees
with what `floor`/`ceiling` need: rakudo ranks a literal argument as native-shaped for *dispatch*, but
a literal is still a *boxed value* at runtime, so `ceiling(3.2e0)` must return `Int`.

The fix special-cases `floor`/`ceiling`/`ceil` (single-argument calls only) in
`literal_native_args_mask` to carry a narrower bit instead: `Compiler::is_declared_native_arg`, the
strict subset of the existing `is_native_literal_arg` predicate that keeps only its two "genuinely
declared native" branches — a plain read of a locally-declared native-typed variable
(`my num $y = ...; ceiling($y)`), or an inline anonymous native declaration used directly as the
argument (`ceiling(my num $ = ...)`, roast's own shape) — and drops its literal/const-fold fallback.
`Interpreter::try_native_floor_ceiling` (in `src/vm/vm_native_dispatch.rs`) reads that bit off
`self.literal_native_args` (published for the call's duration by `exec_call_func_op`) before falling
through to the ordinary native-function table, which still owns the `Num`-preserving arm for the
native case and every other type (Int/Rat/Complex/NaN/Infinite/Instance).

A `VarRef`-tag-based runtime check was tried first and does not work: `normalize_call_args_for_target`
strips a plain-variable argument's `VarRef` wrapper before `try_native_function` ever sees it, for any
callee with no user-declared candidate (every builtin, floor/ceiling included) — confirmed with a
`rust-gdb` breakpoint. The compile-time mask is the only signal that survives to that point.

New regression test: `t/types/numeric/floor-ceiling-num-boxing.t` (12 assertions covering the boxed
literal, boxed `Num` variable, computed-expression, native `num` variable, and inline anonymous
native-declaration shapes, cross-checked against `raku` itself). `roast/S02-types/num.t`'s
`'ceiling(num)'` subtest continues to pass (112/112 subtests). Also verified directly against the
`Graph::MinCuttish` repro from the issue: mutsu now agrees with rakudo (`9`, `(Int)`).
