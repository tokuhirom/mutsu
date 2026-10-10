# Callable parameter signatures follow Rakudo's variance

Binding a block to a `&c:(Str:D $p, Regex $pat, ...)` parameter now checks the
expected parameter types against the candidate's the way Rakudo does: a
candidate taking `Regex:D` binds to an expected `Regex`, while `Any`/`Mu`
candidates no longer bind to an expected `Int`. Definiteness smileys narrow a
type in signature comparison. Found via `Display::Listings`, whose
`t/000-Display::Listings.rakutest` now passes.
