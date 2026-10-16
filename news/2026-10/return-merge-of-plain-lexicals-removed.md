# Calls no longer copy a callee's lexicals back into the caller

When a routine returned, mutsu used to copy every lexical the callee's frame
had rebound back into the calling frame, a leftover of resolving names through
the chain of callers. A callee writing an outer variable relied on that copy
to reach the variable, and a caller holding an unrelated variable of the same
name could be in its way.

That copy is gone for ordinary variables (ADR-12529 §3, slice 37). Each write
now reaches the variable it means directly: through the variable's shared
cell when a routine or closure captured it, and through an explicit carrier
when its name is only known at run time (`$::($n) = v`,
`CALLER::...BIND-KEY`). Slices 22-36 moved the last uses of the copy onto
those paths first.
