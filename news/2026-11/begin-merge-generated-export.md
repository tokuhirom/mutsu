# A BEGIN phaser installs its unit's `&EXPORT`; the resolver finds a unit's lexical package

Two more pieces of the BEGIN-time RakuAST surface `CompUnit::Util` drives
(#15371):

- **`$*CU.merge-generated-lexical-declaration($import, :resolver, :force)`**
  with a `RakuAST::Declaration::Import` named `&EXPORT` makes the given routine
  the module's export hook, run for its importer once the module body is done
  (`steal-export-sub`: a module re-exports another module's `sub EXPORT`).
  Other names are not installed yet; the call reports that instead of a missing
  method.
- **`$*R.resolve-lexical-constant($name)`** answers a package the unit declared
  lexically before the phaser (`my package EXPORTHOW { ... }`) as the
  `RakuAST::Declaration::Import` that names it, so `re-exporthow` merges into
  the module's own `EXPORTHOW` instead of dying on a fresh one.

The `RakuAST::Declaration::Import.new(:lexical-name, :compile-time-value)` node
these calls take comes from worker 52's branch, rebased here.
