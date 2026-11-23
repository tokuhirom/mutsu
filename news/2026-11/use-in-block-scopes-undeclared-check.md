# A routine imported in a block is undeclared outside it, at compile time

A `use` imports lexically, so `{ use M; }; f()` calls a routine that is not in
scope at the call. rakudo reports `Undeclared routine` at CHECK time, before
anything runs. mutsu's compile-time check used to give up on any unit with a
`use` that might import routines, so the program ran up to the call and died
there with `Unknown function` (#13441).

A `use` below the unit's top level now leaves only its own statement list
unjudged (and the lists nested in it); the rest of the unit is still checked,
so the call outside the block is reported before anything runs. The
undeclared-name check does the same, so `{ use M; }; say Thing` for a term `M`
imports is an undeclared name at compile time, not the string `Thing`. A top-level
`use` still turns the check off for the whole unit, and the `EVAL` form of
the check is unchanged for now.
