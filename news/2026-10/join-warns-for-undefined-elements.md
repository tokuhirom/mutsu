# `.join` warns for undefined elements

Joining an undefined element now raises Rakudo-compatible, resumable string-context warnings while preserving the empty-string result. Warnings are emitted once per element, and a named array's variable name is included when available. The pure join handlers now decline these values consistently so the interpreter can issue the warning.
