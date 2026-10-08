# A pointy block in a map/gather block captures the enclosing `&func` parameter

`sub o(&func) { gather { take inner(-> $n { func $n }) } }` (and the `.map({ ... })`
equivalent) called the running callee's same-named `&func` instead of the captured
one, recursing forever (Red 0.2.5's `transpose-grep`, #12384).

The map/grep callback and the gather body are recompiled at run time by a fresh
compiler that did not know the `&`-lexicals the block was written under, so a call
`func $n` in a nested closure was not recorded as a capture. The recompile is now
seeded with the origin's `outer_code_var_names`, the gather's compiled body inherits
its analysis closure's vouched captures, and the tree-walk gather forcing paths run
the already-compiled body instead of recompiling the AST.
