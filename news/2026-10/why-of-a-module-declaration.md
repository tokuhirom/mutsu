# `.WHY` finds the declarator docs of a declaration in a loaded module

A module's `#|` / `#=` docs were dropped when its load ended, so `.WHY` on a routine, class,
method or role declared in a module was `Nil` once `use M` had returned. That held from the
importer (`&inc.WHY`) and from the module's own routines (`sub w is export { &inc.WHY.Str }`)
alike, with and without `unit module` and with the precompilation cache.

The docs of each loaded module are now kept under the module's compilation unit, and `.WHY` on
a routine reads the table of the unit it was declared in. Two modules that document a routine
of the same name, and an importer that declares its own, therefore answer independently, and a
module that loads another keeps both. A module with no documented named declaration costs
nothing.
