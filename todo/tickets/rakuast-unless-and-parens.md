# RakuAST renders `unless` as a negated `if`, and drops parentheses

Two read-direction divergences measured against rakudo 2026.07. Both render
*something* rather than refusing, so both are silent wrongness.

## 1. `unless`

```
$ mutsu -e 'say Q{unless 1 { 2 }}.AST'
  RakuAST::Statement::If.new(
    condition => RakuAST::ApplyPrefix.new(
      prefix  => RakuAST::Prefix.new("!"),
...
# rakudo:
  RakuAST::Statement::Unless.new(
    condition => RakuAST::IntLiteral.new(1),
    body      => ...
```

mutsu desugars `unless X` to `if !X` and `Stmt::If` keeps no flag saying which
keyword the source used, so `unless 1 { }` and `if !1 { }` are indistinguishable
by the time the converter sees them. (`until` had the same shape but *did* keep
an `is_until` flag, which is why it could be fixed — see
`news/2026-09/rakuast-until-loop.md`.)

The fix is an `is_unless` flag on `Stmt::If`, mirroring `Stmt::While::is_until`.
`Stmt::If {` appears at ~101 sites, so it is a mechanical but wide change; the
flag has no execution meaning, exactly like `is_until`.

Note rakudo's node is `Statement::Unless` with a `body` field (not `then`), and
an `unless` cannot carry `elsif`/`else`, so the node is simpler than `If`.

## 2. Parentheses

```
$ mutsu -e 'say Q{my $x = (1, 2)}.AST'
    ... RakuAST::ApplyListInfix.new(infix => ",", operands => (...))
# rakudo:
    ... RakuAST::Circumfix::Parentheses.new(
          RakuAST::SemiList.new(
            RakuAST::Statement::Expression.new(
              expression => RakuAST::ApplyListInfix.new(...)
```

rakudo keeps the parentheses as a `Circumfix::Parentheses` wrapping a `SemiList`;
`(1)` gets the same wrapper around a bare `IntLiteral`. mutsu renders the wrapper
for `Expr::Grouped` but the parser does not produce `Grouped` in an initializer
position, so the parens are simply gone.

Worth checking whether the parser can keep `Grouped` there, or whether the
initializer path unwraps it deliberately for a reason the converter should
respect.

## Why they are grouped here

Both are "the parser erased a distinction raku keeps", both are in very common
syntax, and both are cheap *once* the upstream representation carries the
information. Neither needs new measurement: the rakudo shapes above are recorded.
