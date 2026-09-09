# Direct regex aliases preserve subrule `.made` across alternation

Grammar captures written as `$<part> = <rule>` now retain the action-produced
`.made` value when the alias appears inside an ordered alternation. This also
works when the same alias names different rules in different branches, as in
`$<part> = <text> || $<part> = <code>`.

## Root cause

The alias and the original subrule capture share one `CapNode`, but the
sigil-prefixed alias path did not record which rule's action should dispatch for
that node. When the alias entry was visited before the original rule-name entry,
the action walk treated it as an actionless capture and the shared node never
received its `.made` value. Alternation wrappers also dropped the alias metadata
while reshaping inner capture deltas.

## Fix

The matcher now tags each direct-alias capture node with its original rule name,
and all alternation/group delta paths preserve capture alias metadata. Dispatch
therefore remains node-specific and does not confuse repeated aliases that select
different rules.

Pinned by `t/grammar-direct-alias-made-alternation.t` (8 assertions), including
both branch orders, fallback to the second branch, and repeated mixed matches.
