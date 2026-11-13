# A sigilless term over a missing element keeps the element's container

`my \x = %h<missing>` binds the term to the element's auto-vivifying container
in rakudo, so `x =:= Any` is False, and so is `c =:= Any` inside a
`sub f(Mu \c)` called with `x`. mutsu answered True for both: the term's slot
held the deferred vivification token, but `=:=` only counted a promoted cell as
a container, and `WrapVarRef` handed a raw callee the token's read value (`Any`)
instead of the token, so a routine-local term (`my \y = %g<z>; f(y)`) arrived as
a bare value.

Both now treat a deferred token over a mutable `Hash`/`Array` as the container
it denotes. A missing key of an immutable `Map` stays a plain value, and its
deferred token now reads as `Nil` (`Map.AT-KEY`) rather than `Any`, so a missing
`Map` key passed straight to a raw parameter is `=:= Nil` as in rakudo (#17656).
