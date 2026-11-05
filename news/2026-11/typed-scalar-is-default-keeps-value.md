# `my TYPE $x is default(D)` keeps its default and initializer

The deferred type registration of a typed scalar with an `is default` trait
treated an env-free slot as uninitialized and overwrote it with the type
object, so `my Real $p is default(1); say $p` printed `(Real)` and
`my Int $r is default(3) = 7` lost its `7`. It now consults the slot named by
the preceding trait op. Found by the raku-doc differential
(`Language/variables.rakudoc`).
