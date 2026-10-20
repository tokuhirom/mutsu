# A `sub EXPORT` hook sees its module's constants when the first `use` is in a block

A module first loaded by a block-scoped `use` ran over a scoped env whose own tier held the module's lexicals. The hook's `UNIT::` read the chain's tail as the module scope, so a `my constant &name` was missing and `use M "name"` bound nothing to the block (`MY::<&name>:exists` was `False`). A module's `UNIT::` now reads every tier of its env chain, so the constant is found whatever the importer's nesting.
