# A module's `constant` and type markers no longer fill the importer's env

Declaring a `constant` leaves a companion marker so that regex `$name`
interpolation and `EVAL` can tell the name is a compile-time constant, and a
typed declaration leaves one recording its type constraint. A module body runs
in the importer's env, so every marker its file-scope declarations wrote stayed
behind in each frame env of the program that loaded it — after
`use Cro::HTTP2::RequestParser`, 45 entries every copy-on-write deep copy of a
frame env had to copy, many of them orphaned once the binding they described
had already been taken out of the importer's scope.

A `constant` marker a module's mainline writes now goes to a per-package table
that the two readers consult after the env, so a `unit module`'s markers stay
visible to its own routines and no longer to the importer. The type markers of
a `unit module`'s file-scope bindings now leave the importer's env together
with the bindings, and the importer's own markers under the same names are
restored. A `Promise(supply { whenever … })` loop under that `use` deep-copies
21% fewer env entries per iteration (6,405 → 5,085).

This is the third slice of ADR-0084 (#7817).
