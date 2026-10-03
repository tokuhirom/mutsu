# An optional positional now out-ranks a slurpy for a zero-arg multi call

`multi g(*@a)` and `multi g($x?)` tie on minimum arity, and Rakudo ranks the
non-slurpy one narrower, so `g()` answers the optional candidate. Two bugs hid
this in mutsu. The sub resolver's exact-arity fast path returned a lone
slurpy candidate before the optional-arity candidates (registered under a
different arity) were ranked; it now skips the fast path when the exact set
holds a slurpy. The method ranker charged an unfilled *untyped* optional
positional a distance of 1000 while a typed one cost nothing, so the slurpy won;
an untyped positional that receives no argument is now not compared at all.
Closes #11173.
