`RegexTree` now preserves unprefixed and dot-prefixed static `before`/`after`
assertions through RakuAST, including the named capture suppression difference
between `<before ...>` and `<.before ...>`. Constructed nodes use the same
zero-width execution path.
