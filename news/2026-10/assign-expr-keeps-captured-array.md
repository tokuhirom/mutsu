# Expression-form array assignment keeps captured containers

`(@a = ...)` or `@a = @b = ()` on an `@`/`%` variable captured by a named sub replaced the
variable's slot instead of storing through its shared cell, so the sub and the mainline saw
different arrays. The expression-form assignment now writes through the cell like the statement
form. Found with Algorithm::Diff, whose `t/base.rakutest` now passes 61/61.
