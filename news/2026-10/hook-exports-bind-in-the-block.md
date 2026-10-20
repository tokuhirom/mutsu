# A `sub EXPORT` hook's values bind in the importing block

The names a module's `sub EXPORT` hook returns under a literal key of its
`Map` (`'$x' => ...`, `'@log' => @log`, `'vrai' => vrai`) are now pad
declarations of the block that holds the `use` (ADR-8751, #14140), as the
module's `is export` variables and constants already were.

- The parse-time module scan records those keys
  (`export_hook::collect_export_hook_value_keys`) as `hook` entries of the
  module's exported variables. The compiler declares each one after the
  block's `UseModule`, and the binding reads its value through the new
  `OpCode::GetHookExport`.
- The import stores the hook's value in the `use`'s lexical import set
  (`ModuleState::lexical_import_hook_values`) instead of writing an env
  alias, so nothing is left for the import scope to roll back.

A closure or sub taken in the block now keeps those values after it exited
(it read `Any` and the bareword string before), an imported `$x` hides an
outer `$x` in the block, and a loop body that `use`s the module sees it.

The BEGIN-time preload of a module only a nested block `use`s no longer
installs the hook's values into the unit's env. The block's own `use` re-runs
the hook where the import belongs; the preload's copy stayed visible after
the block (`if False { use M }; EVAL '$x'`).

An uppercase sigilless key (`'MyAlias' => Int`, usually a type) and every
name a hook computes keep the run-time import for now.
