# A module's top-level constants leave the frame env

A module's top-level `constant` is published under its package-qualified name
(`Pkg::VERSION`, and `&Pkg::alt` for `constant &alt = &alternatives`). That
name was meant to live only in the package-symbol table since #11882, but the
cross-thread publication that followed the store put it back into the
importing program's frame env, so every frame and every closure capture kept
carrying it. A constant never changes after its declaration, so it now skips
both the env and the shared store, and the qualified code-variable and call
paths find `&Pkg::name` in the table.

This is the first slice of ADR-12529 phase 1. On the FunctionalParsers EBNF
parse, the module's twelve `constant &` shortcuts left the shared layers of
almost every closure capture, and by-name chain walks fell 19%.
