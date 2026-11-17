# A module whose CHECK phase rewrites its `$*CU` runs from the rewritten tree

Red's `red-compile` rewrites the `.grep`/`.map`/`.sort` calls of the unit it
is called from so they carry a compiled SQL template (`:red-ast(...)`). A
program that says `CHECK red-compile` got its queries compiled, but a module
that did (Red's own test fixture `CompilerCallableFixture`, and any
application module built on Red) did not: the module's compile phase never
looked at the tree its CHECK phase had changed, so its queries quietly fell
back to symbolic evaluation (Red t/124, "Precompiled callable templates retain
routine identity").

A module's statements now get the same end-of-compile-phase marker the main
program and an `EVAL` unit have: the closures `$*CU.add-check-code` queued run
there, and a tree the CHECK phase rewrote becomes the rest of the module's
mainline. Unlike the main program's restart, which runs the tree as an EVAL'd
unit, a module's restart is lowered and run as the module's own mainline, in
the module's scope, so what the tree declares is the module's: its exported
and `our` routines replace the first compile's (a restart's declaration is
not a redeclaration), the `unit module` the mainline already entered is not
nested in itself, and the first compile's frame takes the tree's file-scope
lexicals before it publishes its slots, so the module's routines read them
once the load is over.

The CHECK-time code object of a `sub` declared in a `unit module`
(`compile-time-value` of a call to it) also carries that package now, not
`GLOBAL`, so its `static_id` equals that of the routine the module installs:
Red's guard that the callable it compiled is still the one being called no
longer rejects every routine of a module.

Red 0.2.13 `t/124-compiler-callables.rakutest` now passes all 76 assertions.
