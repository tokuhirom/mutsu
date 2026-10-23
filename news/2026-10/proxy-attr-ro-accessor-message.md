# A read-only accessor over a Proxy-bound attribute reports the fetched value

Assigning to `$obj.attr` where `has $.attr` is bound to a `Proxy` (as AttrX::Mooish does for
lazy attributes) died with `Cannot modify an immutable Proxy (Proxy)`. Rakudo's accessor hands
back the attribute's value, so the refusal now names what the Proxy FETCHes
(`Cannot modify an immutable Str (is baz)`).

The same change makes a `:D` / `:U` attribute enforce its smiley on assignment: `$!n = Nil`,
`$obj.n = Nil` and a type object stored into `has Int:D $.n` now die with
`X::TypeCheck::Assignment` as in Rakudo (the smiley was only checked at construction).

A scalar attribute bound to a typed container (`$!val := $container`) now enforces that
container's own type and `:D` smiley on `$!val = ...` (AttrX::Mooish's store-value relies on it).
