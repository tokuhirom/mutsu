# A module that merges another unit's GLOBAL hands its symbols to its importers

A module can merge another unit's GLOBAL into its own at BEGIN time with
`GLOBALish.WHO.merge-symbols($cu.handle.globalish-package)`, as CompUnit::Util's
`steal-globalish` does. In rakudo, whoever then `use`s that module sees those
symbols, in the scope of its `use`.

mutsu now does the same:

- The `globalish-package` of a file-system unit names that unit.
- Merging it during a module's load records that the module re-exports the
  unit's GLOBAL.
- Merging the module into an importer, at the unit's top level or in a block,
  also merges every unit it re-exports.

CompUnit::Util 0.7.0's `t/03-rexport-globalish.rakutest` passes (#15371).
