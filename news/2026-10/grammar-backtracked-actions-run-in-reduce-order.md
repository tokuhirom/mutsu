# Backtracked grammar reductions now run their actions in reduce order

A grammar action fires when its rule reduces, so a reduction that a later alternative supersedes
(the same rule matched again over the same span) still runs. mutsu replayed those superseded
reductions before the whole match-tree walk, so for `<bits>* [ <expression>+ ]* %% <bits>+` over
`{+path}/here` the trailing `bits` action ran before the `expression` one and `.made` came out as
`(/here, expr)` instead of Rakudo's `(expr, /here)`.

The superseded reductions are now parked and flushed from inside the walk, just before the first
surviving node that starts at or after them and before the enclosing node's own action. This puts
`URI::Template`'s `t/020-grammar.t` (and the `{+path}/here` family of cases in `t/030`, `t/040`)
back in order.

The rest of those files still fails on a class-body `my &f` that a class-body `my sub` reads as
the caller's `&f`; that is tracked as #10651.
