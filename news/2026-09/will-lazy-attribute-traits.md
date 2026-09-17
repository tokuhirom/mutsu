# `will lazy { ... }` attribute traits receive their block

mutsu now preserves the block in an attribute trait such as
`has $.value will lazy { 42 }`, passes it positionally to the `trait_mod:<will>`
multi, and passes `:lazy` as its named marker. Attribute-trait role mixins also
run their `compose` hook and preserve wrapped accessor dispatch, matching
Rakudo's behavior.

This allows `Attribute::Lazy` 0.0.7 to load and its complete test suite to pass.
