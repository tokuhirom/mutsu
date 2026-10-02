# Class-body writes and binds share an outer lexical's cell

A class body statement that assigns an outer lexical (`class F { $z = 4 }`)
compiles to a package-qualified store (`SetGlobal("F::z")`). When another
class's method had captured `$z`, the variable lived in a shared cell, but the
store landed on `F::z` and the per-statement copy-back then replaced the cell
in env with the plain value — so the method kept reading the old value and a
later outer write never reached it (#11086). The qualified store is now
redirected to the bare name while a class/role body is being walked and that
name holds a shared cell, so it writes through the cell like any other
captured-lexical write.

A class-body bind to an outer lexical (`class E { my $w := $z }`) was a value
copy for two reasons: the block-final `VarDecl` arm of `compile_block_inline`
(every class-body statement is its own chunk, hence block-final) dropped the
`:=` lowering, and the VM's bind path recognized an outer source only through a
saved call-frame env, which a class-body chunk run through `run_nested` does not
have. The tail arm now keeps the full bind lowering, and a source visible only
through env while a class/role body runs is treated as an outer lexical: the
alias and the source share one cell, and the declaring frame's slot adopts it
through the caller-var writeback.
