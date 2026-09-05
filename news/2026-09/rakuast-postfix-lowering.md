# RakuAST postfix lowering, and a silent no-op increment

`EVAL` of a RakuAST tree handled only three of the postfix forms the read
direction renders — `Call::Method` (with no dispatch modifier), `Call::Term`,
and `Postcircumfix::ArrayIndex`. Everything else in the cluster read fine and
then failed with `EVAL does not yet support lowering RakuAST::ApplyPostfix`.

## Change

`src/rakuast/lower.rs`'s `ApplyPostfix` arm now covers:

- `Postfix(operator => "++" / "--")` -> `Expr::PostfixOp`. Unlike `Infix` and
  `Prefix`, a `Postfix` carries its operator in a *named* `operator` field, so
  it needs its own reader.
- `MetaPostfix::Hyper(Call::Method)` -> `Expr::HyperMethodCall`, the node the
  converter already renders `@a>>.abs` from.
- `Call::Method`'s `dispatch` field -> the `.?` / `.+` / `.*` modifier. It was
  being dropped: `EVAL(Q[my $x = -5; $x.?no-such].AST)` threw
  `No such method` where the source form returns `Nil`.
- `Call::QuotedMethod` -> a method call with a quoted name. Only a
  single-`StrLiteral`-segment name lowers; an interpolated one is a different
  internal node.

## The bug underneath

Adding the postfix operators surfaced a real defect in a shared table.
`op_name_to_token_kind` had no row for `++` or `--`, so both fell through to its
`Ident` catch-all. A *prefix* increment already lowered — `++$x` renders as
`ApplyPrefix(Prefix("++"))` and reached `Expr::Unary` — but with
`op: Ident("++")`, which the compiler does not treat as an increment. It
compiled to something that quietly produced `Any`:

```
$ mutsu -e 'use MONKEY-SEE-NO-EVAL; EVAL(Q[my $x = 1; ++$x; say $x].AST)'
1     # before
2     # after
```

No error, no boundary message — the increment simply did not happen. The table
is used only by the RakuAST lowerer, so the two added rows change nothing else.

## Coverage

`t/rakuast-eval-postfix.t` (12 assertions) pins postfix and prefix `++`/`--`
including their differing result values, an increment on an array element, a
C-style loop whose step is `$i++`, hyper method calls, `.?` on both a present
and a missing method, and a quoted method name. It is a dual-oracle test: it
passes verbatim under both mutsu and raku.
