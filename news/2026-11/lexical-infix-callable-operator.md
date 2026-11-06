# A lexical `&infix:<op>` shadows the builtin operator

`sub f(&infix:<cmp>, $a, $b) { $a cmp $b }` and `my &infix:<cmp> = ...` now
make the operator syntax (`$a cmp $b`, `* cmp *`, a `sort` block) call the
lexical binding, as rakudo does, instead of only the explicit
`&infix:<cmp>($a, $b)` form. The compiler tracks the `&infix:<op>` lexicals in
scope and lowers a matching binary operator to a call of the binding; the sort
comparator fast paths no longer read the source shape of a block whose
compiled body does not apply the builtin `cmp` / `<=>`. This is what
BinaryHeap's `heapsort` with a custom comparator needs.
