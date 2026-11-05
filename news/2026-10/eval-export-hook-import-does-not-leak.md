# An `EVAL`'s `sub EXPORT` hook import no longer outlives it

The `use` of a module that imports through a `sub EXPORT` hook is pre-run before
an `EVAL`'d unit runs and dropped from the unit, so the unit had no import
region of its own and the hook's terms stayed bound in the caller's env. A later
`EVAL` of a `use` whose arguments export no such term still read the earlier
one (`Logic::Ternary`: `True` kept naming `Logic::Ternary::True` after
`use Logic::Ternary <none>`). The pre-run now binds in an import region that
ends with the `EVAL` (#15770).
