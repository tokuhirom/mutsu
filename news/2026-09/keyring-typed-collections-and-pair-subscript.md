# Keyring can use typed collection defaults and Pair subscripts

Keyring 0.2.0 now passes both of its baseline test files under mutsu. Its
`Backend:U @.backend-priority` attribute contains backend type objects, so the
definedness smiley must constrain the elements of a typed collection rather
than rejecting the collection object itself. Mutsu now checks positional
elements and associative values accordingly, including nested shaped arrays.

Keyring also uses Pair-valued subscripts. User-defined `AT-KEY(Pair:D)` methods
now receive those Pair keys during reads, matching the existing assignment and
existence dispatch.

The regression coverage is in
`t/types/coercion/typed-defined-container-attr-default.t`.
