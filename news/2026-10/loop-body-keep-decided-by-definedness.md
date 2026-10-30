# Loop-body KEEP/UNDO is decided by definedness and a trailing assignment keeps its value

A `for`/`loop` body with a `KEEP` or `UNDO` phaser chose between them by the
*truthiness* of the body's last value, and treated a trailing assignment
(`$n += 1`, `$x = 5`) as an undefined `Nil`. `gather for @v { KEEP { take $_ };
$n += 1 }` therefore ran `UNDO` on every iteration, and `$n++` did the same on
the iteration where the old value was `0`. Both now follow Raku: the decision is
`.defined` of the body's value, and a trailing assignment yields the assigned
variable. Found with the `annotations` distribution (#15295).
