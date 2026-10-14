# A package stash lists an enum's keys under their bare names

`module M { enum E <ea eb> }; say M::.keys` printed `($ea $eb E)`; Raku prints `(E ea eb)`. An enum
key declared in a package is bound under a qualified scalar-shaped env key, and the stash built the
member name from that shape. The stash now recognises a key that belongs to an enum of the package
(or to the enum whose own stash is being built) and publishes it as the term it is, the way a
sigil-less `our constant` already was. `M::E::.keys` is `(ea eb)` as well.
