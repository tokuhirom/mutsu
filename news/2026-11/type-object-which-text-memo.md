# A type object's `.WHICH` text is rendered once

`Int.WHICH` and every other type object's identity text (`Name|U<id>`) is a
function of the type's name alone, yet each call rebuilt it with `format!`,
the user-facing name rendering and the identity-name normalization. The text
is now memoized per thread and name; every call still answers a fresh
`ObjAt` carrying it. `M.WHICH` on a sigilless parameter went from ~17.2K to
~14.9K instructions, and Monad's `M.WHICH ~ '|' ~ T.WHICH` cache key from
~44.6K to ~39.5K (#17331).

Found on the way: #17596 (`A.WHICH =:= A.WHICH` is True; rakudo says False).
