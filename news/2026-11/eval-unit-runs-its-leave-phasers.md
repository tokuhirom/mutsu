# An EVAL'd unit runs its LEAVE / KEEP / UNDO phasers

`EVAL 'LEAVE { say "leave" }; say "body"'` printed only `body` when the EVAL was
called from the mainline: the unit's exit phasers were wired up only when the
compiled code was a routine body, so a unit of its own never ran them (inside a
`sub` they did). An EVAL'd unit is a scope whose exit phasers run when it ends,
wherever it was called from, so the unit now takes the phaser-scope path too.
`variable-trait-block-add-phaser.t` (a trait adding a unit-level `LEAVE` through
`$v.block.add_phaser`) joins the `EVAL($ast)` ratchet.
