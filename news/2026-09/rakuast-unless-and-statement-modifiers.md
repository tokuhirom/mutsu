# RakuAST renders `unless` as `Statement::Unless`, and postfix `if`/`unless` as condition modifiers

`unless X { … }` was rendered as a `Statement::If` over an `ApplyPrefix("!")` —
mutsu's own desugaring showing through, not raku's shape. Since it rendered
*something* rather than refusing, it was silent wrongness in one of the most
common statement forms in the language:

```
$ mutsu -e 'say Q{unless 1 { 2 }}.AST'      # before
  RakuAST::Statement::If.new(
    condition => RakuAST::ApplyPrefix.new(prefix => RakuAST::Prefix.new("!"), …
# rakudo:
  RakuAST::Statement::Unless.new(condition => RakuAST::IntLiteral.new(1), body => …
```

## The flag was the whole fix

`Stmt::If` kept no record of which keyword the source used, so `unless 1 { }`
and `if !1 { }` were indistinguishable by the time the converter saw them.
`Stmt::While` had already met this exact problem and solved it with an
`is_until` flag (`news/2026-09/rakuast-until-loop.md`); `Stmt::If` now carries
the same-shaped `is_unless`. It has no execution meaning — `unless X` and
`if !X` run identically — so it is set at the two `unless` parse sites and
`false` at the other nineteen `Stmt::If` constructions, and propagated by the
three passes that rebuild an `If` in place.

With the keyword recoverable, the converter strips the `!` the parser added
(the existing `strip_negation`, shared with `until`) and emits
`Statement::Unless`, whose block field raku names `body` rather than `then` and
which cannot carry `elsif`/`else` — rakudo rejects those at compile time, which
mutsu's parser already does too. The lowerer re-plants both halves, so the write
direction round-trips.

## The postfix forms are a different node again

Measuring `unless` turned up a second divergence in the same statement family:
raku does not wrap a postfix-modified statement in a conditional statement at
all. It hangs the condition off the statement itself:

```
RakuAST::Statement::Expression.new(
  expression         => …,
  condition-modifier => RakuAST::StatementModifier::Unless.new(RakuAST::IntLiteral.new(1)))
```

mutsu rendered a whole `Statement::If` around it — wrong for `unless` *and* for
plain `if`, which nothing had noticed because the `if` case at least got its
condition right. `Stmt::If` already tracked `is_statement_modifier` (it has real
execution meaning: a modifier introduces no block, so a `state` in its branch
belongs to the enclosing block), so both spellings were recoverable together.
The converter now mirrors the `given`-modifier arm that was already there,
emitting `StatementModifier::If` / `StatementModifier::Unless`, and the lowerer
reads them back.

## Scope

The ticket this closes also asked for parentheses to survive as
`Circumfix::Parentheses`. That half turned out to be a different size of
problem — the parser builds no `Expr::Grouped` in the reported position, or in
several others, and wrapping unconditionally is a semantic change across every
consumer that pattern-matches the unwrapped shape. It is re-filed with its
measurements as `todo/tickets/rakuast-parentheses-not-preserved.md`.

`without` still renders as a negated `if` (raku has `Statement::Without`); it is
lowered through a different path and was not part of this ticket.

Pinned by `t/rakuast-unless.t`, whose 16 assertions pass identically under
rakudo 2026.07 and mutsu.
