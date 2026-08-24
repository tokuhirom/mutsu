# Nested tuple targets now work with hyper assignment

Hyper assignment with a nested tuple target, such as
`(($a, $b), $c) «=» ((1, 2), 3)`, now recursively assigns each RHS element to
the corresponding target.

The parser previously excluded bare `=` from the general hyper-operator path.
That left `«=»` to a statement-level special case which only recognized indexed
targets, so a parenthesized tuple was parsed as three unrelated sink-context
expressions. Hyper assignment is now lowered recursively to the existing native
VM assignment operations. Its RHS is stored in a compiler-visible temporary so
that nested extraction evaluates the original expression exactly once.

Regression coverage includes nested and multiply nested targets, mixed hyper
arrow directions, and RHS evaluation count.
