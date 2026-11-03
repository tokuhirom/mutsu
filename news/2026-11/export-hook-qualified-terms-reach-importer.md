# A `sub EXPORT` hook's qualified terms reach the importing module's routines

A module whose `sub EXPORT` returned `Map.new: 'Graph::Grid' => Graph::Grid` made
`Graph::Grid.new(...)` die with "Default constructor for 'Any' only takes named
arguments" inside the routines of a module that `use`d it (Graph::RandomMaze).
Three things were wrong, and each is fixed:

- the hook ran in the importer's package, so a `class` it declared was nested
  under it (`Graph::RandomMaze::Graph::Grid`); it now runs in the package it was
  written in (GLOBAL), as in rakudo;
- a package-qualified term handed to a module was filed under the hook module's
  own load instead of the importer's, so it was missing from the importer's
  scope once its load was over;
- that scope table refused every qualified key, and the importer's own
  file-scope pad treated the term key as a variable and stored `Nil` for it.

Pinned by `t/modules/import-export/export-hook-qualified-term.t`. The
distribution's remaining difference (a second `use` of the hook module) is #16283.
