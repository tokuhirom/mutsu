# A `.=` application carries origins wherever it is written

`VAR .= name` was placed only as a statement of its own. As a call argument
(`say(@a .= reverse)`, `is-deeply @a .= sort, ...`) its parts had no range, and
inside parentheses (`say ($a .= lc)`) neither did the `SemiList` and the
`Statement::Expression` around it. `call_origins::postfix_call` now hands an
`ApplyDottyInfix` to the same placement, and a bracketed list of one statement
whose expression is an unplaced `.=` takes the trimmed text inside the
brackets as that expression's range (`list_origins::place_lone_dotty`). On the
54-file differential, nodes rakudo has and mutsu lacks went from 2217 to 2141,
with no new mismatches. A quoted method name (`$a .= "$b"()`) is still
unplaced. Part of #15780.
