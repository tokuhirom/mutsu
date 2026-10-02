# An INIT/CHECK that reads a routine's own lexical keeps its write

`sub f { my $z; INIT $z = 5; $z }; say f()` printed `(Any)`: the `INIT` was lifted out of the
routine to run once at program start, and its body then named `$z` where no `$z` existed. The
same went for `CHECK`, for methods and for blocks, loops and closures.

The nested-`BEGIN` machinery of ADR-0134 slice 2 now serves `INIT` and `CHECK` too. The phaser
runs in the unit's own `INIT`/`CHECK` sequence inside a block that declares the lexical from a
unit-level static cell and copies it back, and the routine's declaration starts from that cell on
every call, as rakudo's static pad does. A `BEGIN` and an `INIT` of the same scope share one cell.
A method's phaser re-enters its class (and so sees the class body's lexicals in their static
state); a role's reads nothing the role declares.

A phaser that reads nothing of an inner scope is untouched, and a failed lift of one never stops
the `BEGIN` lifting. Still open: a method of a class declared inside a routine, and the value of a
tail `INIT` statement. See the ADR-0134 section for #10562.
