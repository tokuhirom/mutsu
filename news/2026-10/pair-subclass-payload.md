# Subclasses of `Pair` now construct and answer the Pair methods

`class ValuePair is Pair { ... }` (the ecosystem `ValuePair` module) used to recurse forever
in `self.Pair::new($key, $value)` and, without a `new` of its own, die with "Default constructor
only takes named arguments". A Pair subclass is now a tagged instance carrying the built Pair
under `__mutsu_pair_value`, the same native-payload convention as `is Version`: `.new` takes
`(key, value)` or `:key/:value`, `self.Pair::new(...)` from the subclass's own `new` builds the
payload, and `.key`, `.value`, `.kv`, `.antipair`, `.raku`, `.gist` and friends run on it.

Found by the `immutable` distribution; its remaining failures are tracked in #12485 and #12486.
