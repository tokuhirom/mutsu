# A closure's return copies back only what the closure itself wrote

When a closure returns, mutsu used to walk all of the closure's variables
and copy the changed ones back over the caller's. It now copies back only
the variables the closure itself assigns, and dynamic variables. Everything else already reaches
the caller through the variable's shared container: the closure's own
declarations, writes made by routines it calls, dynamics. What remains of
the copy-back covers a write the creating code could not prepare a
container for, such as a block called inside a grammar subrule's argument
(`<x({ $calls++; 2 }())>`), which is compiled while the match runs.
ADR-12529 §3, slice 43.
