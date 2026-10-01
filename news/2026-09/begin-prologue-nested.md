# A nested BEGIN runs once, at BEGIN time

This is slice 2 of [ADR-0134](../../docs/adr/0134-begin-time-prologue.md).
Before this change, a `BEGIN` inside a routine, closure, loop or block ran
when the enclosing code ran. So it ran every time that code ran, or not at
all: `sub f { BEGIN say "x" }` printed nothing, and
`for 1..3 { BEGIN say "b" }` printed `b` three times. A value-form BEGIN
inside a block was evaluated at the block's first execution, and
`{ BEGIN 42 }()` returned `Nil`.

Now each such BEGIN is lifted into the unit's BEGIN prologue, ahead of the
top-level statement that contains it, and runs exactly once. The enclosing
scopes' lexicals are in their static state there. A lexical of an inner scope
gets a static cell: the lifted body reads and writes the cell, and the inner
declaration starts from the cell's value on every entry. As a result,
`{ my $x = 2; BEGIN say $x.raku }` prints `Any` and
`sub f { my $x; BEGIN $x = 5; $x }` returns `5`, as on rakudo.

A nested `constant` that reads such a lexical is lifted with it. Some BEGINs
keep their old handling: one in a package body, one whose scope declares a
routine or type ahead of it, and `will begin`. Once one of these is met, no
later BEGIN is lifted, so source order holds.
