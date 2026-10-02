# An INIT/CHECK in a method of a class declared inside a routine keeps its write

`sub f { my class K { method m { my $z; INIT $z = 5; $z } }; K.m }; say f()` printed `(Any)`.
The static-cell treatment of #10562 stopped at a class declared inside code, since the lifted
phaser re-enters a class by name and such a class does not exist at the unit's level until the
code runs.

A phaser that reads only its own method's lexicals needs no package, so the walk now enters a
class (or role) declared inside code as a detached scope: its routines are walked, and the phaser
is lifted to the unit's `INIT`/`CHECK` sequence with the same static cells, without re-entering
anything. A phaser that reads `self`, an attribute, a `$?` variable or a name the type declares is
left where it was. This covers `my` and package-scoped classes, roles, classes nested in them,
multi methods and `CHECK`.

Still open ([#10711](https://github.com/tokuhirom/mutsu/issues/10711)): an `INIT` of such a class
that reads nothing of its method still runs when the enclosing code runs the declaration, rather
than at program start.
