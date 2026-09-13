# A qualified callsite no longer leaks its `Pkg::` prefix into `&?ROUTINE.name`

Found while writing the regression cover for #7766 unit 2 (PR #8346): a
`Pkg::sub()` callsite reached its own routine frame as the name `Pkg::sub`
instead of `sub`, while an unqualified call to the same routine correctly
named it `sub`.

```raku
package P { our sub f() { &?ROUTINE.name } }
say P::f();
```

Both `raku` and mutsu already agreed that `&?ROUTINE.package` answers `P`
here. Only `&?ROUTINE.name` disagreed: rakudo answers `f`, mutsu answered
`P::f`.

`&?ROUTINE` resolves through `resolve_code_var`'s `?ROUTINE` arm, which reads
the current `RoutineFrame`'s `name` field directly off `routine_stack` — the
frame `push_routine_with_location` pushes on every call, whether the general
by-name dispatch entry, the light-call fast paths, or the OTF-compiled-call
caches. Every one of those callers hands the push whatever text the parser
attached to the callsite's name constant, so a qualified call passed the
qualified text straight through as the routine's own name, with only the
general dispatch entry happening to resolve the routine's defining *package*
back out of `resolve_function_with_types`/the compiled function's own
`package` field first. Fixing only that one caller was not enough: a second
call to the same routine is served by the light-call cache, which pushes its
own frame directly and never revisits the general entry at all, so the bug
resurfaced on every repeat call.

`push_routine_with_location` now strips any `Pkg::` qualification off `name`
itself before storing it on the frame — the one place every call path
converges on before `&?ROUTINE.name` can read it, so the fix holds
regardless of which dispatch entry served the call.

`t/routines/signature/param-bind-symbol-keys.t` gains two assertions: a
qualified callsite's `&?ROUTINE.name`/`.package`, and the same call repeated
to exercise the light-call cache.

[#8347](https://github.com/tokuhirom/mutsu/issues/8347)
