# Roles of one name with different parameter lists coexist in an EVAL'd unit

The redeclaration check of an `EVAL` unit keyed a role by its name alone, so `role R[::T] {...}; role R {...}` (a role group)
was refused as `Redeclaration of symbol 'R'`. A role is now keyed by its name and the shape of its parameter list, as the
candidates of a role group are. The `EVAL($ast)` ratchet pins the 10 more files this lets through.
