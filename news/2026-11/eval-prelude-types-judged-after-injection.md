# An EVAL'd unit sees the prelude types it asks for before it is judged

A built-in type mutsu declares from a prelude (`OperatorProperties`,
`NumericEnumeration`, `Tappable`, ...) was only injected after the unit's
CHECK-time checks ran, so an `EVAL` that was the only place naming it died with
`Undeclared name` (string `EVAL` and `EVAL($ast)` alike). The preludes are now
injected before the checks, and a prelude class or role an earlier unit already
declared is not declared again. Four more files join the `EVAL($ast)` ratchet.
