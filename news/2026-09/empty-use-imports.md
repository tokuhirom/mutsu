`use Module Empty` and `use Module ()` now load the module without importing
its exports, matching Raku's empty import-list semantics.
