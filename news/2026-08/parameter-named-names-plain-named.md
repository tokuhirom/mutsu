# `Parameter.named_names` now reports the primary name of plain named parameters

`collect_named_names` (`src/value/signature.rs`) only populated `named_names`
when a named parameter had an alias sub-signature (`:x($a)`, `:x(:y($a))`) —
for a plain `:$x` it returned an empty list, where rakudo reports `("x",)`.
The alias-chain order was also outermost-first; rakudo reports innermost-first
(`:z(:w(:v($b)))` gives `("v", "w", "z")`). Both are fixed; slurpy `*%h` and
capture parameters still correctly report no named_names.

This was the direct cause of 18 of the 19 remaining `Cro::HTTP`
`http-router.rakutest` failures: `Cro::HTTP::Router`'s `compile-route` builds
its request-unpack code from `$param.named_names[0]` for every named route
parameter (`get -> 'search', :$min-price is query = 0 { ... }`), so the
generated matcher looked up `Q[]` — the empty string — in the query/header
data for every named parameter, and every handler ran with its defaults
instead of the request's values. With the fix, `http-router.rakutest` goes
from 64/83 to **82/83** (the remaining failure, "Two optional segments handled
correctly", is an unrelated pointy-block optional-positional-defaults bug;
the file also still aborts at test 83 because mutsu's `.UInt` coercion of a
negative value throws instead of returning a soft `Failure` that would let
the route's signature-bind check reject it — both filed separately).

Pinned in `t/parameter-introspection.t` (plain named, single alias, slurpy,
pointy-block param, and the rakudo innermost-first alias order — all verified
against `raku` directly).
