# The target of an assignment statement has its range in `.AST`

`$n = $n.abs if $n.WHAT` left the condition's invocant and postfix without a range: the assignment
target `$n` was read by the statement parser without noting where, so the terms after it took the
ranges in the wrong order. The target is noted now, and the equal terms after it match `raku`.
Closes #16656.
