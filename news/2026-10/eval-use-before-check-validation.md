# EVAL: a hook-importing `use` runs before the CHECK-time validation

An `EVAL`'d unit is validated for undeclared names and routines before any
statement runs, so a top-level `use` of a module that imports through a
`sub EXPORT` hook had its names guessed from the module's source text:
`EVAL q[use M; Thing]` died with "Undeclared routine: Thing" while Rakudo,
which runs `use` at BEGIN time, answered 42. Such a `use` (the module declares
a `sub EXPORT`, no `:if` condition) now runs first, with its argument list,
inside the EVAL's import scope and in position for LEAVE-phaser attachment,
and is dropped from the unit so the hook runs once. The checks then read the
real import set; a name the hook did not bring in is still
`X::Undeclared::Symbols`. A `use` nested in a block remains approximated
(#13441).
