# A narrow native-int attribute's ++/-- now wraps on overflow

`$!attr++`/`$!attr--` (and the public `is rw` accessor form `$.attr++`)
never wrapped a narrow native-int attribute (`int8`, `uint8`, ...) on
overflow, unlike an equivalent `my int8 $x` lexical or `my int8 @a` array
element, which both already wrapped correctly.

The read-modify-write tail (`wrap_native_int_arithmetic_result_for` in
`src/vm/vm_value_helpers.rs`) looked up the variable's type constraint only
through the routine-scoped `__mutsu_type::<name>` env entry -- the lane a
`my` declaration registers in its own frame. An attribute has no such entry:
its declared type lives in the class registry instead, which is exactly why
the plain-assignment store path (`exec_set_local_op_inner`) already falls
back to `scalar_attr_type_constraint` for `$!attr = ...`. The
increment/decrement tail was missing that same fallback, so
`has int8 $.v = 127; $!v++` silently produced `128` instead of wrapping to
`-128`.

`wrap_native_int_arithmetic_result_for` now falls back to
`scalar_attr_type_constraint` when the env-scoped lookup finds nothing,
mirroring the plain-assignment path. Pinned by
`t/nativecall/native-int-attr-incdec-wraps.t`.

While investigating, a second, unrelated wrap gap was found: a compound
assignment through a method-call lvalue target (`$.v += 1` when `.v` has no
local slot) does not consult the accessor's declared type at all -- it
shares its root cause with mutsu issue #9005 (`.=` through a method-call
target), not with this fix, so it was left for that issue rather than
folded in here.

Closes #8985.
