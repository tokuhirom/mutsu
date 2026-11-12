# A single `for` parameter gets a local slot

`for @list -> $x { ... $x ... }` bound its parameter by name in the
environment, and unless an enclosing scope already gave the name a slot,
every read of `$x` in the body was a by-name scope walk (`GetGlobal`). It is
the commonest loop shape there is; EEL's state copier reads its loop
parameter several times per attribute (#17530).

The compiler now gives the parameter a slot for the loop. The VM already
binds a loop parameter into its slot as well as the environment when the slot
exists, so the body reads it with `GetLocal`. After the loop the slot is
retired, as a multi-parameter loop's fresh names are, so a later read of the
name is the enclosing binding again. A body that declares the same name again
(a nested block's `my` would share the slot) or declares a routine (an
`our sub` closes over the parameter by name) keeps the by-name binding.

A ten-element loop reading its parameter three times went from 167k to 140k
instructions (release build, callgrind).
