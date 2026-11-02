# A `given` inside a LEAVE/KEEP body no longer replaces the block's value

`my $v will leave { 1 } = 5; $v` as the tail of a block answered the phaser's value (`1`) instead of
`5`. `will leave` lowers to a `LEAVE` whose body is a `given` over the variable, and a statement
`given` nets a value on the stack that the LEAVE/KEEP/UNDO sections never popped, so it was returned
in place of the block value. The phaser sections now sink such statements, which also fixes a
hand-written `LEAVE { given ... { ... } }` (#15613).
