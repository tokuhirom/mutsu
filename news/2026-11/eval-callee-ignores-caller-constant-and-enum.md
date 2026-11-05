# A callee's `EVAL` no longer reads its caller's `my enum`

`sub callee { EVAL q[Red] }` called from `sub outer { my enum Col <Red Green>; callee() }` returned
`Red`; `raku` reports it as not visible. The static link that stops a top-level routine's
reflective lookup at its own scope covered only names its `lexically_scoped` test knew, and the
bare enum-key namespace was not among them. It now is, so the lookup continues at the program
scope, past the callers.

An uppercase `my constant` (`\SECRETC`) still leaks. Making its term key lexical broke a loaded
module's routines, which read the names they imported through the caller chain
(`t/nativecall/carray-import-type-check-in-module-sub.t`); that, `my subset` and `$?ROLE` stay on
#16099.
