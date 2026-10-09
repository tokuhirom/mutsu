# A precompilation hit decodes a routine body only when the routine is used

A module loaded from the precompilation cache used to decode all of its routine bodies
and clone each into its `FunctionDef` while registering it, although a program calls a
handful. The cache now stores each body as a length-prefixed byte string. A decoded table
keeps the bytes in a `LazyFn` slot, and `FunctionDef.compiled` became a `RoutineBody`
that decodes the body and adapts it to the def on first use (ADR-12026 §2.2).

`use Test; ok 1;` decodes 2 of the module's 62 routine bodies. The load cost in a debug
build fell from 282.7M to 245.5M instructions (callgrind, minus an empty script). The
verify and round-trip sweeps over `t/modules`, `t/routines` and `t/oo` are clean.
