# A bare imported-routine call no longer leaks its value: `ExecCall` retired

`.map({ f 1; $_ * 2 })` answered `(42 42 42)` instead of `(2 4 6)` when `f`
was a wrapped routine imported from a module. It did so only on the first run
after the module was compiled. Once the module came from the precompilation
cache, the same program was correct (#9448).

A bare statement call to an imported routine (`f 1;`) compiled to a dedicated
`ExecCall` opcode, not the `CallFunc` + `SinkPop` pair that every other
statement call uses. `ExecCall` was documented and emitted as "no push", but
two of its branches pushed the call's value anyway: the wrap chain and
NativeCall. Nothing popped the value, so it became the value of the enclosing
block. The dependence on the cache came from the parser. It treats `f 1;` as
a statement call only when it knows `f` is imported, which it did only when
the module was compiled from source in the same process.

The fix removes the opcode rather than patching the two pushes.
`exec_exec_call_op` was a second copy of call dispatch. Its own comments
recorded four earlier bugs where `CallFunc` was right and `ExecCall` was
wrong: NativeCall, a shadowed builtin, a `sub EXPORT` hook and a sunk
Failure. A positional statement call now compiles to the same `CallFunc` the
expression form uses, followed by `SinkPop(false, true)`. That is what the
"normalized" statement calls (`push`, `pop`, ...) already did, so there is one
call path, and a sunk Failure still throws. The two other emitters (the
`Test::More` plan call and `use newline`) moved to the same shape.
`exec_call` and `exec_call_values`, which only `ExecCall` reached, are gone
with it.

`ExecCallPairs`, the statement call carrying named arguments, stays for now.
Retiring it is the rest of #9448's plan and needs `CallFuncNamed` to cover
its callsite-line and carrier special cases first.

Regression test: `t/modules/import-export/imported-statement-call-leaves-no-value.t`
runs each snippet in a fresh subprocess against its own module directory, so
the first run is always a cold-cache run.
