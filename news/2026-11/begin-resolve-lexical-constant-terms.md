# BEGIN-time `resolve-lexical-constant` sees what a loaded module installed

`$*R.resolve-lexical-constant($name)` at BEGIN time now finds two more kinds
of symbol in the importing unit:

- a package that a loaded module vivified under `GLOBAL`, as CompUnit::Util's
  `set-unit('GLOBALish::Foo::Bar', ...)` does;
- a sigilless term that a `sub EXPORT` merged into the unit or block, as its
  `set-unit` / `set-lexpad` do.

Its `compile-time-value` reads such a term directly, without evaluating its
name.

An `EVAL`'d unit's `use` of a hook module now also installs the declarations
the hook merged into its importer. The EVAL pre-run records them alongside
the hook's returned map and replays them too.

CompUnit::Util 0.7.0's `t/08-get.rakutest` goes from 2 to 4 of 5 (#15371).
The last test needs a nested block's own BEGIN scope (#17515).
