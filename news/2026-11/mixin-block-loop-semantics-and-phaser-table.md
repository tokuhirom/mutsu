# A Block with a role mixed in iterates as the Block; `Block`'s `$!phasers`

`.map` and `.grep` treated a Block with a role mixed in (`{ ... } but R`) as an
opaque callable: `last` inside it escaped as a bare `X::ControlFlow`, and its
`NEXT`, `FIRST` and `LAST` phasers never ran. Unless a mixed-in role composes
`CALL-ME` (which the loop now calls), the mixin is iterated as the Block itself,
as rakudo does, and so also takes the inline loop path a plain Block gets.

`nqp::getattr($block, Block, '$!phasers')` now answers the block's phaser table
(phaser name to list of callables), and `nqp::bindattr` of a table on a block
with a role mixed in replaces the block's loop phasers for the loops that
iterate it. ParaSeq's `BlockMapper` rewires that table on each batch's clone so
that a hypered `map`'s `LAST` runs once, after the last batch: its
`map({ PHASERS; $_ * 2 })` now reports `NEXT` once per element and `LAST` once,
and `map({ last if $_ == $n; ... })` stops where rakudo does instead of hanging.
