# A routine declared in a class body writes the file-scope lexical it names

`my $n = 0; class C { sub bump { $n++ } method go { bump() } }` left `$n` at 0:
the class body registers its `sub`, `my sub` and `my method` at run time, with
no closure-creation op, so nothing told the declaring frame to box `$n` in a
shared cell the way it does for a method that writes it (#15782). The compile
pass that already harvests a class body's methods now does the same for its
routines, and a method's own writes box the lexical too, so a code object of
the method sees the same cell. With that, a class-body `my method` that writes
an enclosing variable no longer needs the tree-walk carrier (the Proxy-subclass
`STORE` in `t/vm/writeback/proxy-subclass-new-lvalue-assign.t`). A slice of
#14572.
