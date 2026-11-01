# A compound assignment to an element with a literal or variable subscript needs no temporary

`%h{$k} += 1`, `%h<k> += 1` and `@a[$i] *= 2` were expanded into a block that
declared a `__mutsu_idx_N` temporary for the subscript, read the element
through it and stored back through it: a `DoBlockExpr`, a `SetVarDynamic` and a
`SetLocalDecl` per execution on top of the read and the store, about 12-14k
instructions against 4.5k for `%h{$k}++`. When the subscript is a literal or a
plain scalar variable the expansion now reads and stores through the subscript
itself: both evaluations come before the right-hand side runs (the store's index
is pushed first), so a right-hand side that rebinds the variable still meets the
subscript the statement started with. Every other subscript (a call, an
operator, `$k++`) keeps the temporary, so it is evaluated once, and the
short-circuit operators (`//=`, `||=`, `&&=`) are unchanged. Pinned by
`t/collections/subscript/compound-element-assign-subscript-forms.t`.
