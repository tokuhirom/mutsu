# A captured multi dispatcher answers its key-decided checks once per argument-type key

`captured_multi_plan_winner` (an imported `my proto sub f(|) {*}` called through its value) cached
the ranked candidate stage per (candidate set, argument keys) but still re-bound every candidate on
each call. It now builds a `StageProgram` from the first call with the key, as the by-name multi
path does, and extends it to candidates the old program refused: named, slurpy and sigilless
signatures, and calls with named arguments. A candidate whose signature has no `where`, `subset`,
type capture, literal value or code signature has a verdict that the argument-type key (types,
definedness, declared types, named-argument names) decides, so the general matcher answers it once
and the call only runs the value-dependent checks (#15786). Such a program is never taken by the
multi light call, which binds plain positionals.
