# `bless` now applies container coercion to attribute initializers

Attribute initializers reached through a custom constructor calling
`self.bless(...)` now follow the same `@`/`%` container coercion rules as the
ordinary `.new` path. A multi-pair default for a hash attribute therefore
becomes a `Hash`, rather than remaining an `Array` and failing at its first
associative lookup.

This makes `WWW::DuckDuckGo` 0.1.1's complete test suite pass under mutsu.
