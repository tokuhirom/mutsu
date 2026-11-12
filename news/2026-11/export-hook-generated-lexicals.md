# A module's `sub EXPORT` can install declarations into its importer's unit or block

CompUnit::Util's `set-unit` / `set-lexpad` install a symbol into the
compilation unit or the lexical block that `use`s a module, through
`$*CU.merge-generated-lexical-declaration` and `$*R.current-scope`. mutsu
now supports both from a `sub EXPORT`:

- A merged declaration is queued while the hook runs and installed with the
  hook's returned map once the importer's environment is back.
- Inside a block, `$*R.current-scope` is that block, so `set-lexpad` stays in
  it. `set-unit` reaches the whole unit through the BEGIN-time preload of the
  block's module.
- A block's `use` re-runs its hook when the block runs, after the unit's
  compile phase. It now still sees `$*CU`, as it would at compile time in
  rakudo.
- `$*R.get-global` is `GLOBAL`, the package the bare `GLOBALish` term names.
- A `use lib` whose argument is a path chain the parser can fold
  (`$?FILE.IO.sibling('lib').Str`) is in force for those preloads, like a
  literal one.
- A routine bound into an export tag's stash through an expression
  (`$WHO<DEFAULT>.WHO<&foo> := ...`, `set-unit('EXPORT::DEFAULT::&foo', ...)`)
  is exported.

CompUnit::Util 0.7.0's `t/06-set.rakutest` goes from 0 to 7/7, matching
rakudo, and `t/08-get.rakutest` from 0 to 2/5 (#15371).
