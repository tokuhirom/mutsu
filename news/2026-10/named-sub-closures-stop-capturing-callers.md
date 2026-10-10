# A closure made in a named sub no longer captures the sub's callers

A closure created inside a closure body already stopped collecting names at
that body's frame. A closure created inside a named sub did not: its capture
still took in every frame that had called the sub, all the way down. Those
callers' names are lexically invisible to it, and they made the capture grow
with the depth of the call.

It now stops at the named sub's own frame and keeps only the program scope
below it. The sub's free variables never came from its callers anyway: they
resolve through the declaration-scoped stores. On the FunctionalParsers EBNF
parse, closure captures carry half as many layers as before. This is part of
ADR-12529 phase 3.
