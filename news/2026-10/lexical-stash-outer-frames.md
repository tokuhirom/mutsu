# `LEXICAL::` sees enclosing scopes' variables

`LEXICAL::` was compiled exactly like `MY::`, so it held only the current
frame's lexicals: `{ my $x = 1; { say LEXICAL::<$x> } }` printed `Nil` where
Rakudo prints `1`. It now lists the target frame and every frame enclosing it
— enclosing blocks, an enclosing routine, and the file scope across a routine
boundary — with an inner declaration shadowing an outer one of the same name.
Outer entries are read through the same captured-lexical path as
`OUTER::<$x>`, so a later write to the outer variable is seen.
