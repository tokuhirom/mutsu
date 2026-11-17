# An EVAL'd unit runs the LEAVE phaser a variable trait added

A unit-level variable trait can add phasers to the unit's block
(`$v.block.add_phaser("LEAVE", { ... })`). An `EVAL`'d unit, string or tree,
never ran such a LEAVE: the replay phasers the trait lifting queues were
excluded from the unit's phaser scope, because running them there lost the
unit's value after a `trait_mod:<does>` mixin.

The value was lost because an EVAL'd unit returns its tail on the VM stack
while the phaser scope routed it through `$_`, so the unit answered whatever
else the stack held. The scope now pushes the tail as the unit expects
(`PhaserBlockResult::Push`), and the replays are no longer excluded.
`t/oo/trait/variable-trait-block-add-phaser.t` passes as `EVAL(.AST)` and joins
`ci/rakuast-ast-eval-passing.txt`. Closes #17202; the regression test is
`t/oo/trait/eval-unit-var-trait-leave.t`.
