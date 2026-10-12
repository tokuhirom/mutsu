# A routine owns its `$/`; a block shares its scope's

Every Raku routine declares its own `$/`, initially `Nil`, and a block uses
the `$/` of the scope it is written in. mutsu already kept a routine's match
out of its caller, but a routine *read* its caller's: `sub f { $/ }` answered
the caller's last match, and `m:c` in a sub continued from the caller's
position instead of 0 (#12398). In the other direction, a `.subst` or
`.match` inside a block that another routine called did not reach the
block's scope (#12720).

This is ADR-12529 phase 3, slice 10. A routine's frame root now owns the
match state, and a lookup of `$/`, `$0` or `$<name>` stops there instead of
walking into the callers; a block's frame answers `$/` from its capture. A
block that touches the match state in any way (a `.match`, `.subst`, a read
of `$/`, not only a regex `~~`) shares its scope's `$/` through a cell, and
one setter for match results stores through that cell, so a block made
before its scope matched again still reaches the scope's `$/`. No per-call
work was added: the lookup is selected by a chain latch, and which code
touches the match state is decided at compile time.
