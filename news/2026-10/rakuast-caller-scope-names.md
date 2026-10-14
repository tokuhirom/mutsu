# EVAL and `Str.AST` see the names of every enclosing scope

Under `MUTSU_RAKUAST=1`, an `EVAL` string that named a constant of an outer
scope was refused when the program also declared a routine:

```raku
constant b = 256;
{ say EVAL 'b' }         # 256
sub f { }
```

The scan that tells the RakuAST converter which bare names the calling scope
declares read only the current frame, and a block whose frame does not copy
its outer scope lost the constant. It now walks every enclosing scope, and
also sees a constant declared in a block that has already ended, which is
`our`-scoped.

`Str.AST` resolves names against its caller the same way, as rakudo does:
`constant b = 1; say q[b].AST` now renders `b` as a `RakuAST::Term::Name`
instead of refusing the bareword, and a class the caller declares is a
`Term::Name` too.

Three more `t/` files pass under `MUTSU_RAKUAST=1`.
