# `cas` and the `⚛` operators no longer walk the general call resolution

`$x⚛++`, `atomic-fetch`, `cas($x, ...)` and the other atomic forms lower to a
`CallFunc` of a native helper (`__mutsu_atomic_*` / `__mutsu_cas_*`). Those names
live in a reserved namespace the compiler emits and no user routine is declared
in, yet every such call went through the whole of `exec_call_func_op_inner`: the
frame-lexical, increment, NativeCall and empty-proto gates, the light-call and
OTF cache probes, the `CALL-ME` mixin probe, the wrap chain, the lexical
override, `has_proto`, `find_compiled_function_memo`,
`user_only_sub_hides_builtin`, `imported_env_aliases` and finally
`try_native_function`. All of it misses for these names, and it cost ~5-8k
instructions per call to reach a 4-6k handler (#12120).

The names now carry a memoized symbol flag, `flags::NATIVE_ATOMIC_HELPER`, the
way `nqp::` ops carry `NQP_OP`, and `exec_atomic_helper_call_op` hands the call
straight to `try_native_atomic_function` with the arguments prepared as the
general path prepared them: the target of an `_var` helper keeps its `VarRef`
tag (`flags::ATOMIC_TARGET_HELPER`), every other argument unwraps, the callsite
marker is stripped, `Proxy` operands are FETCHed, and the `is rw` writeback
drains afterwards, on the error path too. A call with an argument-source
descriptor (`|EXPR`, named) keeps the general path, and a name that the helper
table does not answer is an internal error rather than a silent miss: a unit
test pins that every name in `NATIVE_ATOMIC_HELPER_NAMES` has a handler.

Callgrind on the three `cas` loops of `roast/S17-lowlevel/cas-int.t` (4 threads
x 10000 iterations, release, per iteration):

| target | before | after |
| --- | ---: | ---: |
| `my atomicint $v` | 33.9k | 29.0k (-14.6%) |
| `has atomicint $.v` | 41.4k | 36.6k (-11.6%) |
| `my atomicint @v[2;2]` | 41.1k | 35.0k (-14.8%) |

The whole file went from 5.70 s to 4.80 s on a 4-core container (five
alternating runs each, both sides incremental release builds of `main` and of
this branch). rakudo runs it in 1.5-2.6 s on the same box, so #12120's goal
(within 1.5x) is still open; what is left per iteration is the handler's own
by-name work (`atomic_target_arg`, the `check_readonly_for_modify` interns, the
attribute key `format!`) and, under all of it, the ~17k instructions per
iteration a typed `loop` body costs with no atomic at all (#12151).
