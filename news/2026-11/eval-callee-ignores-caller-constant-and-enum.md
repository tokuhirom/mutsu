# A callee's `EVAL` no longer reads its caller's `my constant` or `my enum`

`sub callee { EVAL q[SECRETC] }` called from `sub outer { my constant SECRETC = 5; callee() }`
returned `5` (and `Red` for a caller's `my enum`); `raku` reports both as not visible. A lowercase
`my constant` and a `my class` were already hidden: the static link that stops a top-level
routine's reflective lookup at its own scope covered only names its `lexically_scoped` test knew,
and an uppercase-initial term key (`\SECRETC`) and the bare-enum-key namespace were not among
them. Both are now lexical names, so the lookup continues at the program scope, past the callers.

`my subset` (the registry keeps the bare name globally) and `$?ROLE` still leak; see #16099.
