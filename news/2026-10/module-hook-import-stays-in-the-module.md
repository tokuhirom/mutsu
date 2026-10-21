# A module's own hook imports no longer reach the program that uses it

A module that imports a `sub EXPORT` hook's values at its own top level (`use
JSON::Fast;` inside `unit module M;`) now keeps those names to itself. Before,
the hook's `&routine`, `$scalar`, `@array` and `%hash` were written into the
env the module body loads in, which is the importing program's own scope, and
the module load never took them back out. So a program that wrote only `use
M;` could call `to-json`, and `EVAL 'hf()'` or `::('&hf')` found the hook's
routine there too. Rakudo reports the name as undeclared.

The hook path now records such a name in the loading module's scope
(`module_imported_names`), as the `is export` variable path already did. The
load's restore takes the name back out of the loader's env, and the module's
routines find it in the module's scope tables. A sigilless term stays where it
was, because its key is shared with a same-named `$scalar`. Part of #14140.
