# A `:m` callee with a conjunction runs as a markless frame

A whole-`:m` rule called as `<r>` over a subject that has combining marks runs
as a markless frame of the compiled engine, entering its ends one at a time as
rakudo does. A conjunction (`&&`, `&`) in the body used to decline that
compile and take the eager `:m` path, running code blocks once per end
computed (`3,2,1` where rakudo logs `3`).

The conjunction's other branches now run as markless nested runs over the same
subject, as a look-around's body already does; their captures are positions in
that subject and need no mapping (#13176).
