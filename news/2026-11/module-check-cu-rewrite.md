# A module's CHECK-time `$*CU` rewrite takes effect

Rakudo compiles a compilation unit from its `$*CU` once the CHECK phase is
over. A CHECK phaser or a `$*CU.add-check-code` closure that rewrites the tree
therefore changes what the unit runs. mutsu honoured that for the main program,
which restarts from the rewritten tree. It silently ignored it for a module:
the module was never restarted, so it, its importers and its exports kept the
first compile.

A module now marks the end of its compile phase too: after its last top-level
CHECK phaser, or after its BEGIN prologue. When the CHECK phase has changed the
tree, the module's first mainline ends there. The loader then runs the module
body again from the lowered tree, in the same load, env and package as the
first run:
- its BEGIN prologue and CHECK phasers do not run a second time;
- its routines and types replace the first compile's instead of redeclaring
  them;
- a top-level `my` without an initializer keeps what the CHECK phase wrote into
  it;
- what it exports is what importers see.

The CHECK-time `compile-time-value` of a routine declared in a `unit module X`
file now belongs to `X`, as the routine the module then declares does, so the
two compare equal by `static_id`.

This is what Red's `CHECK red-compile` relies on. In a module it attaches
compiled query templates to the module's `.grep` calls. Red's
`t/124-compiler-callables` now passes 76/76, as on rakudo.
