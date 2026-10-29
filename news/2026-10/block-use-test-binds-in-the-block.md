# A block's `use Test` binds in the block's import set

`Test` and `Test::*` had their own compile arm (and an exclusion in
`lexically_importable_module_name`'s ancestor `preloadable_module_name`) that
emitted `UseModule` with no lexical import set, so a `use Test` below the unit's
top level installed `proto sub skip` and the rest as registry aliases that only
the import scope's rollback took back out. A block's `use Test` now fills the
block's set like any module's `use`; the unit's own top-level `use Test` is
unchanged. This was the single routine-table rollback dependency found by
disabling the rollback over all of `t/` and the whitelisted roast files.
