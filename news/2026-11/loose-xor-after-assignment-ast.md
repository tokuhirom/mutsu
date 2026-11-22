# The loose `xor` after an assignment keeps its spelling in RakuAST

`Q[my $y = 1 xor 2].AST.DEPARSE` gave `my $y = 1 ^^ 2`: the parser reads the
loose `xor` as `^^` at a looser precedence, and the view recovers the written
`xor` only when the application is placed in the source, which a declaration's
re-read variable or an assignment's target never was. A `^^` on the spine of a
tail after an assignment or an initialized declaration can only be the loose
`xor` (a tight `^^` binds inside the assigned value), so the view now spells it
`Infix("xor")` there, as rakudo does; a parenthesized `($x = 1) ^^ 2` keeps
`^^` (#18340).
