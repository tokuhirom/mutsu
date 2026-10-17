# A call's return copies nothing back any more

ADR-12529 retired the last two places where returning from a call copied
the callee's variables back over the caller's: a named routine's write to a
variable the caller had not declared yet, and a closure's writes to the
variables it captured. Every write now reaches its variable through the
variable itself (a shared cell), or for a name known only at run time,
through the frame that declares it.

The last case to need a copy-back was a block created inside a grammar
subrule's argument (`<x({ $calls++; 2 }())>`), which is compiled while the
match runs. Such a block now gets a cell for the variable it writes, and
the caller's variable shares it, so the write lands even when the block
runs later as a rule argument (§3, slice 46).
