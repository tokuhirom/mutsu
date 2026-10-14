# RakuAST renders user operators named `__`

`.AST` refused an application of a user-declared operator whose name starts
with two underscores:

```raku
sub infix:<__>($a, $b) { "$a $b" }
say "a" __ "b";          # a b
say [__] 1, 2, 3;        # 1 2 3
```

The converter treated a `__` prefix as one of the parser's own helper names,
but the parser builds none. The application is now an ordinary
`RakuAST::ApplyInfix` over `Infix("__")`, as in rakudo, and the reduction a
`Term::Reduce`.

Seven more `t/` files pass under `MUTSU_RAKUAST=1`; five of them were
already passing but were not listed.
