# User traits on routines, variables, methods, attributes and parameters run in the declaration phase

A user `trait_mod:<is>` for a routine, a variable (also one with an
initializer, `my $x is tagged = 3`), a method, an attribute or a parameter now
runs when the declaration is compiled, before the unit's run-time
initializers, as in rakudo (#14146). Previously only a type declaration's
traits ran in the BEGIN prologue (#14374); the others ran where the
declaration stood, so `my $flag = 'x'; ... sub f is marked { }; say $flag`
printed the trait's value instead of `x`.

The unit's prologue now treats a declaration as a BEGIN-time effect when it
uses a trait named by a named parameter of the unit's own `trait_mod:<is>`
(`DeferredTraitParents`). Two local tests that encoded the old order now state
the declaration order (`@log.shift`) and drop the initializer the trait ran
before.
