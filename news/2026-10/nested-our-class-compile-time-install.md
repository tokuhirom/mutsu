# `our` classes declared inside routines are installed at compile time

An `our`-scoped class or role declared inside a routine, method, block or
closure used to exist only once the enclosing code had run, so
`sub f { class K { } }; say K` printed the bareword `K` instead of `(K)`.
The unit compiler now walks the whole unit (with the typed AST visitor) for
type declarations nested in code and pre-registers a declaration-only shell
for each at the head of the unit, in source order, qualified with the package it lives in
(`Outer::Inner` for a class declared in a method of `Outer`, `Mod::K` for one
in a sub of `module Mod`). The in-place registration still runs on every
entry of the enclosing code, so the class body's statements keep running at
run time and the type object stays the same (#10470).

A nested shell composes its roles' methods but runs no role body (it
registers in the unit's head frame, where a method a role body block
declares would close over the wrong lexicals); the in-place registration
runs the body once. Rakudo runs it at compile time even if the routine never
runs; that remains open as #10494. Re-registering a parameterized role's
first candidate now also replaces `roles[name]`, so its methods close over
the latest declaring frame instead of the shell's.
