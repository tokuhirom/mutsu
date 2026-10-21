# An EVAL's nested `use` of a `sub EXPORT` hook module runs before CHECK time

`EVAL q[{ use M; Thing }]` failed with "Undeclared routine: Thing" and
`EVAL q[{ use M <U>; nosuch }]` was not judged at all, because only top-level
`use` statements of an EVAL'd unit were run ahead of its CHECK-time validation.
A hook module's `use` nested in a block, branch or routine body is now lifted
to the same pre-run (when its arguments are literals and the unit has no
`use lib` or BEGIN), so the checks see what the hook really imports (#13441).
The import is not confined to the block; the validation was equally lenient
before.
