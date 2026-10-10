# EVAL in a sub no longer sees its caller's lexicals

A frame's env is chained over its caller's, so a name looked up by string
walked on into whatever frames had called the routine. `sub g { EVAL q[$secret] }`
called from `sub h { my $secret = 1; g() }` read `h`'s `$secret`, and so did
a symbolic `::('$secret')` in `g`. In Raku both are compile-time lexical
scope: `g` can see its own lexicals and the program scope it was declared in,
never its caller's.

A named routine declared outside every routine body that looks names up
reflectively now gets a *static link* on its frame root (ADR-12529 phase 3,
slice 3): a plain lexical that misses the frame continues at the program
scope, skipping every caller frame. Dynamic variables, `CALLER::` and the
topic resolve through the callers as before. `EVAL $code, context => $ctx`
keeps the whole chain until a `PseudoStash` carries the lexicals of the
frame it names.
