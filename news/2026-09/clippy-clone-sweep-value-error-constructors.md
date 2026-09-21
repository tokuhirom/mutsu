# Removed 39 wasted clones from `src/value/`'s error constructors

`cargo clippy --all-targets -- -W clippy::redundant_clone` flagged 79 hits across
`src/value/error_typed.rs`, `src/value/error_construct.rs`, `src/value/mod.rs`, and
`src/value/signature.rs`: the same shape repeated dozens of times, a `String` message built
once with `format!()`, then `.clone()`d into an error-attributes map or a second constructor
call even though the original binding was never read again afterward. `msg.clone()` /
`msg.to_string()` on the last use became a plain move (`msg`), removing one `String`
allocation per call site.

39 of the 40 unique call sites were genuine — verified by confirming clippy's "this value is
dropped without further use" note against the surrounding function body, then rebuilding.
The 40th, `src/value/which_id.rs`'s `cloning_mints_a_fresh_id` test, was a false positive:
`WhichId`'s `Clone` impl deliberately mints a fresh id rather than copying the existing one
(`Self::default()`, not a derived `Clone`), so replacing `a.clone()` with a move of `a` would
have changed the test from exercising that custom behavior to aliasing the same id — left
untouched.

Closes [#8908](https://github.com/tokuhirom/mutsu/issues/8908).
