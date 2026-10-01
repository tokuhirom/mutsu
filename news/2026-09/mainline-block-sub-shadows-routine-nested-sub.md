# A mainline-block `my sub` no longer reads a same-named routine-nested sub's variables

A `my sub reset` in a bare block of the mainline called after a routine that declares its
own `my sub reset` ran `reset()` against the routine's free variables: the latest-activation
cell table is keyed by (sub name, variable) and owned by (package, file), so the routine's
cells answered for the unrelated block sub. Declaring a same-named sub with no routine-nested
free variables now drops the stale entries (mutsu#10391).
