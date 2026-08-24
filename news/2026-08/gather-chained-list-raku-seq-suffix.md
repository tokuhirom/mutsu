# Direct gather `.list` chains now render as Lists

The non-mutating method-call path left a coroutine-backed `LazyList` unchanged for
`.list` and `.List`, so `(gather { take 1; take 2 }).list.raku` exposed the internal
`Seq` wrapper as `(1, 2).Seq`. The same expression worked after storing the gather in a
variable because that path reified the gather before dispatch.

The direct path now uses the same reification and coercion as the variable path. Empty
and non-empty direct gather chains therefore render as `()` and `(1, 2)`, matching Raku.
Regression coverage is in `t/gather-chained-list-raku-suffix.t`.
