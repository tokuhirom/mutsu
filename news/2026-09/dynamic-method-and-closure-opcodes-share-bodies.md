# Dynamic method calls and closure literals share one body per family

Two families of opcodes each carried hand-kept copies of the same logic, and
the copies had drifted (#9454).

**Dynamic method calls.** `CallMethodDynamic`, `CallMethodDynamicMut` and
`HyperMethodCallDynamic` (`$obj."$name"()`, `$var.="$name"()`,
`@a>>."$name"()`) each re-implemented the statically named call. Only one
reified a Seq invocant, only another FETCHed a Proxy argument to a mutator,
and none of them took the many special cases the static body has grown. Each
now owns only the name resolution. A Callable in the name position
(`$obj.$code()`, `@a>>.&f`) is invoked directly, and a method name dispatches
through the static body (`exec_call_method_named_op`,
`exec_call_method_mut_named_op`, `exec_hyper_method_call_named_op`) with the
run-time spelling.

A run-time name is never a compile-time macro, so it dispatches like a quoted
name: `A.new."$m"()` with `$m = "WHAT"` now calls a user `WHAT` method, as
Rakudo does. Fixing that exposed a bug in the quoted form itself:
`42."WHAT"()` died with "No such method" because a quoted pseudo-method always
skipped the built-in. It now skips it only when the receiver has a user method
of that name. The hyper form had the same defect per element, and now
interns its method name once per call instead of once per element.

**Closure literals.** `MakeAnonSub`, `MakeAnonSubParams`, `MakeLambda` and
`MakeBlockClosure` were four copies of the capture pipeline and the `SubData`
construction. They differed in which steps they ran. Only the AnonSub pair
froze read-only `:=` loop captures, only `MakeLambda` boxed Supply container
captures, and `MakeBlockClosure` never stripped an inherited
`__mutsu_return_type`. None of those differences was intentional, so every kind
now runs every step through one `build_closure` (`src/vm/vm_closure_build.rs`).
The per-kind differences (name, signature, `Block` vs `Sub`, a declared return
type, `WhateverCode`/`Method`, the bare block's `$/` capture) are explicit
fields of a `ClosureSpec`.

Tests: `t/oo/method/dynamic-method-name-matches-static.t` and
`t/routines/closure/closure-literal-forms-share-capture.t`.
