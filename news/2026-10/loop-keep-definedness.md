# A loop body's KEEP/UNDO follow definedness and see an assignment's value

The expanded loop phasers tested the iteration value's truthiness, so a
falsy-but-defined value (`0`, the old value of `$n++` on its first pass) ran
UNDO, and a body ending in an assignment statement (`$n += 1`, `my @a = ...`)
had its value dropped to `Nil`, so KEEP never ran (annotations'
`Buffer.STAMP`, `KEEP { take-rw $binder }`, #15295). KEEP/UNDO now test
`.defined` and an assignment/declaration statement yields the variable's value.
