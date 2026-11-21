# The finalization of a unit takes one top-level statement at a time

ADR-15760 slice 2d, step 2. `rakuast::finalize::finalize_unit` was one walk over the whole parsed unit. It is
now `UnitFinalizer`: `begin` installs the unit's name scopes and reads the inputs that still look ahead (the
routines the unit declares, which are hoisted, the capitalised terms and core-named types its later
statements declare, the `@_` / `%_` it declares), `commit` finalizes one top-level statement together with the
lists nested in it, and `finish` runs the passes that need the whole lowered unit. Each commit opens its own
parser lexical scope, so nothing the finalization registers outlives the statement.

`finalize_unit` is a loop over it for now, so nothing changes in what a program does. The next step lets the
parser commit each statement to both the post-parse passes and the finalizer as it parses it, with the
lookahead inputs settled at the end of the unit.
