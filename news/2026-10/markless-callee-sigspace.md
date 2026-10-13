# A `:m :s` callee runs as a markless frame

A whole-`:m` rule called as `<r>` over a subject that has combining marks runs
as a markless frame of the compiled engine, entering its ends one at a time as
rakudo does. A body that held `<.ws>`, which every `:s` (sigspace) rule does,
used to decline that compile and take the eager `:m` path, running its code
blocks once per end computed (`4,2` where rakudo logs `4`).

`<.ws>` is a call, and a call does not inherit `:m`, so the frame decides it
over the subject's own text and needs nothing else (#13176).
