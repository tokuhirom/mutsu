# The `with` / `without` statement modifiers cross the RakuAST boundary

`EXPR with X` and `EXPR without X` executed correctly, but the parser rewrote
them into a `given` whose body is an `if $_.defined`, so by the time anything
downstream looked at the AST the modifier keyword was gone. `.AST` could only
report a boundary error:

```
$ mutsu -e 'say Q["found" with "hello"].AST.gist'
RakuAST: `.AST` does not yet support this construct: DoStmt(Given { topic: ... })
```

raku keeps both as a `condition-modifier` on the statement, exactly as it does
for `if` / `unless`:

```
RakuAST::Statement::Expression.new(
  expression         => RakuAST::QuotedString.new(...),
  condition-modifier => RakuAST::StatementModifier::With.new(...)
)
```

## Why the converter could not just recognise the shape

The desugar is genuinely lossy. `STMT with X` becomes
`given X { if $_.defined { STMT } }`, and a hand-written
`(STMT if $_.defined) given X` produces the identical internal statement — same
`Given`, same `is_statement_modifier`, same `.defined` test. A converter that
matched on the shape would have to guess, and would render one of the two
wrongly. The information has to survive the parse instead.

So `Stmt::Given` grew a `with_kind: Option<GivenWithKind>` marker, recording
which source keyword desugared into it. This is the same trick `Stmt::If`
already uses for `unless` (a negated condition plus an `is_unless` flag, with
the converter stripping the parser's `!` back off) and `Stmt::While` for
`until`. Execution ignores the marker entirely; only the RakuAST converter reads
it.

## Both directions

- `src/rakuast/convert.rs` renders `StatementModifier::With` / `::Without` as a
  `condition-modifier`, unwrapping the topicalizer and its `.defined` test back
  to the two pieces raku's node actually holds. The expression-statement
  spelling arrives wrapped in a `DoStmt` (that is how the parser keeps
  expression semantics), which has no RakuAST counterpart, so `Stmt::Expr` now
  unwraps it when it carries a marked `Given`.
- `src/rakuast/lower.rs` accepts the hand-built node and rebuilds the same
  `given`/`if` shape the parser produces, marker included, so the round trip is
  stable.
- `src/rakuast/mod.rs` registers the two classes.

Output is byte-identical to rakudo 2026.07 for `"found" with "hello"`,
`"nf" without Nil`, `say 1 with 2` and `say 1 without Nil`, and
`EVAL(Q[...].AST)` agrees on all of them — including the empty `Slip` both
produce when the topic fails the test.

Critically, `(say 1 if $_.defined) given 2` still renders as a
`loop-modifier => StatementModifier::Given`, which is the whole point of the
marker.

## The block forms are a separate slice

`with X { … }` / `without X { … }` were measured at the same time, as the ticket
asked. They have the same problem but a much larger desugar — mutsu produces
`If { cond: <a `__with_tmp_N` declaration>.defined, then_branch: [Given { … }] }`
rather than a marked `Given` — and raku models them as `Statement::With`
(fields `condition` / `then` / `else`) and `Statement::Without` (fields
`condition` / `body`), with `orwith`/`else` chaining on top. That is filed
separately rather than widened into this change.

Pinned by `t/rakuast/rakuast-with-without-modifier.t` (20 tests: node classes,
which modifier slot is filled, the `given` ambiguity case, `EVAL` of the
round-tripped AST, and the source-level semantics). It passes under rakudo
unchanged.
