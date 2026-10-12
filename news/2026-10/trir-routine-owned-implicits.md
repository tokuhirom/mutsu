# A TRIR-compiled sub no longer reads its caller's `$_` or `$/`

A small routine with parameters, such as `sub f($x) { $_ }`, runs on TRIR,
which compiled `$_`, `$/`, `$0` and `$<name>` as free variables and looked
them up by name through the caller's env. Called inside `given 'x' { }`, the
sub answered `'x'` instead of `Any`, and after a match in the caller it saw
the caller's `$/` instead of `Nil`. A routine owns these variables (ADR-12529
phase 3), so TRIR now leaves such a body to the VM path, which gives the
routine its own. No benchmark changed which routines TRIR compiles.
