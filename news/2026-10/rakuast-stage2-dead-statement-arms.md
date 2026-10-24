# `.AST` no longer rebuilds migrated statements from their lowered form

The parser builds these as typed nodes, and `.AST` renders those nodes
directly (ADR-10723 Stage 2):

- conditionals;
- the loop families;
- `given` / `when` / `default`;
- statement modifiers.

A probe over every `t/` file under `MUTSU_RAKUAST=1` confirmed that
`convert.rs` never reached the arms that rebuilt these statements from the
lowered `Stmt::If` / `While` / `Loop` / `Given` / `When` / `Default`. They
are deleted, together with:

- the `with` / `orwith` / `elsif` rebuilders in `convert_conditional`;
- the `ControlPointy` source record, which the loop and `unless` lowerings
  built only for those arms to read.

The one desugared conditional the conversion still reads is the guarded
control transfer that `proceed |c` / `succeed |c` parses into.

The probe found one statement that still took the old path. An `unless`
written as a term (`(unless 0 { 42 })`) had its own desugaring into
`if !COND`, so `.AST` showed a `Statement::If` over a negated condition. It
now goes through the `unless` statement parser and shows
`Statement::Unless`, as in rakudo 2026.09 (`t/rakuast/rakuast-unless-term.t`).
