# RakuAST: an assignment to a subscript crosses the boundary

`@a[0] = 1` and `%h<a> = 1` are the parser's `IndexAssign`, which `.AST`
refused. It was the single most frequent construct stopping a `t/` file in the
`MUTSU_RAKUAST=1` round-trip mode (283 files).

Measured on rakudo 2026.09, the RakuAST shape depends on the subscript. An
assignment to `@a[…]` or `%h<…>` is folded into the postcircumfix as its
`assignee`:

```
ApplyPostfix(operand => Var::Lexical("@a"),
             postfix => Postcircumfix::ArrayIndex(index => …, assignee => IntLiteral(1)))
```

while `%h{…} = 1` keeps an `Assignment` infix over a plain `HashIndex`. mutsu
renders both, and lowers either back to `IndexAssign`, including in expression
position (`my $x = (@a[0] = 5)`). Until mutsu tells `%h<…>` from `%h{…}` (#10654)
every associative subscript renders as `HashIndex`, so an associative assignment
takes the infix form. The `Postcircumfix::*Index` nodes expose `.assignee`.

The round-trip ratchet grows from 984 to 1041 of 5712 `t/` files. Pinned by
`t/rakuast/rakuast-index-assign.t`, which passes under both mutsu and raku.
