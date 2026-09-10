# A module's custom EXPORT and its own package name survive an EVAL'd first load

`#7764` taught the module loader to drop a `Package`-valued env binding left
over from a module's own transitive `use` statements once that module's load
finished, so a file that never `use`d the transitive dependency directly
could not resolve it bare. That fix uncovered two bugs of its own, both
surfacing specifically when a module was FIRST loaded from inside an `EVAL`
(`Test`'s own `use-ok` is `EVAL ( "use $code" )`, so `use-ok 'M'; use M;` hits
both):

1. **A custom `sub EXPORT` ran against the wrong scope.** `sub EXPORT` is
   part of the module's own closure, so a bareword term inside it (e.g.
   NativeLibs' `Map.new('NativeCall' => NativeCall, ...)`) must resolve
   against what the module's own mainline could see. mutsu instead ran
   `EXPORT` against the (already-restored) importing scope's env, by which
   point `NativeCall` had already been stripped back out (owned by no one
   but the module's own transitive `use NativeCall;`). The bareword silently
   degraded to the plain string `"NativeCall"`, which then shadowed the real
   package for every importer — reproducible with a plain `use`, no `EVAL`
   involved at all. Fixed by snapshotting the module's own env right after
   its body finishes running (before the load's restoration strips it back
   down) and running `EXPORT` — both on first load and on a later re-`use`'s
   rerun — against that snapshot instead of the caller's env.

2. **A module's own bare package-name binding wasn't tracked for
   reinstatement.** `module_package_globals` (and its
   `reinstate_module_package_globals` counterpart) exists precisely to put a
   module's bindings back when a scope loses them wholesale, but it only ever
   recorded `::`-qualified env keys — never the module's OWN bare name (a
   `unit module Foo;` binds bare `"Foo"` in env). So when a module's first
   load happened inside a nested call frame whose own env overlay is
   discarded on return (a sub wrapping an `EVAL`, exactly `use-ok`'s shape),
   the module's bare name vanished with that frame while `loaded_modules`
   kept it recorded as loaded. A later real `use` of the same module is then
   a no-op reuse that never re-installs the missing binding, so the module's
   own name became permanently unresolvable. Fixed by also keeping the
   module's own bare name in the tracked set.

Both are pinned: `t/module-export-sub-sees-module-scope-not-caller.t` for the
first, `t/module-first-loaded-in-eval-keeps-own-package-name.t` for the
second, using minimal fixtures under `t/lib/Issue7806/` shaped after
NativeLibs' actual `use NativeCall; sub EXPORT(|) {...} unit module
NativeLibs:...;` structure. Closes #7806.
