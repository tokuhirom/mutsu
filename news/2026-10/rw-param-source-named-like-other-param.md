# An `is rw` argument no longer clobbers a same-named parameter of the callee

Binding an `is rw` / `is raw` scalar parameter to a caller variable installed a shared cell and a
sigilless alias under the CALLER's variable name in the callee's frame. When another parameter of
that signature was spelled like the caller's variable — `sub f(Str $a, Int $b is rw)` called as
`f("x", $a)` — the callee's own `$a` was replaced by the caller's value (`f` saw `42`, not `"x"`),
and `$b++` wrote through the alias into the callee's `$a`. The binder now leaves the cell and the
alias out when the caller's variable names another parameter of the same signature; the value still
flows back to the caller through the rw writeback. Found through Red's `Red::AST::Eq` operators
(`infix:<==>(Red::AST $a, Numeric() $b is rw)` called as `.a == $a`).
