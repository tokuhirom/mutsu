# Math::Trig tagged code aliases survive a loaded-module re-import

`Math::Trig` 0.5.1 loaded successfully, but importing its `:great-circle`
tag after an earlier `use Math::Trig` lost the `our &great-circle-bearing` and
`our &great-circle-midpoint` aliases. mutsu now resolves durable `our &name`
values from the routine registry when rebuilding a tagged export.

The distribution moves from partial (5/6 baseline files, 207/209 assertions)
to green (6/6 files, 209/209 assertions). Pinned by
`t/modules/import-export/exported-alias-reimport.t`.

This run was locked on [#7884](https://github.com/tokuhirom/mutsu/issues/7884).
