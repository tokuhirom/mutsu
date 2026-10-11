# EVAL in a closure resolves through the closure's own scope

`my $v = 'outer'; my &c = -> { EVAL q[$v] }` called from a routine with its
own `$v` read the routine's `$v`, because a closure's captured scope sat
*below* its callers in the lookup chain. The same held for a closure made in
a callee: `sub k { my $hidden; mk() }` leaked `$hidden` into the closure
`mk` returned, since the capture copied `k`'s frame along with `mk`'s.

A closure that looks names up reflectively (`EVAL`, `::('$x')`) now links its
frame to its lexical outer (ADR-12529 phase 3, slice 4). A closure made at
program scope resolves a lexical through the live program scope; one made
inside a routine resolves it through its capture first, which is now built
from the routine's own scope only. The routine's scalars that such a closure
can read are shared cells, so a write the routine makes after creating the
closure is still visible to it.
