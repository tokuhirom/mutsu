# RakuAST: assignments to literals, nested compound assignments and `$(a; b)` round-trip

Under `MUTSU_RAKUAST=1` (ADR-10723, #7564) these constructs used to stop at a
"desugared do-block" refusal, because `.AST` met the parser's execution-only
expansion instead of what was written:

- `120 = 3` / `"a" = 3` — an assignment to an immutable literal. The parser and
  the lowering now build the `X::Assignment::RO` expansion through one shared
  constructor (`literal_assign_ro_expr`), and `.AST` takes it apart again into
  `ApplyInfix(literal, Assignment, value)`, which is what rakudo prints.
- `($a //= 42) += 10`, `(($a += 2) *= 3) -= 1`, `(COND ?? A !! B) OP= V`,
  `COND ?? A !! B //= V` — the outer compound assignment (and the inner,
  parenthesised one) now carries the same `CompoundAssign` marker every other
  compound assignment does, so it renders as `MetaInfix::Assign` and lowers
  through the same expansion. This also fixes a silent miscompile: `($a += 42) += 10`
  used to be rendered as a call to a prefix operator named after the parser's
  identity helper and died on the round trip.
- `$( my $x = 3; $x + 1 )` and the interpolated `"…$(a; b)…"` — a statement
  list in item context is `Contextualizer::Item(StatementSequence(..))`, built
  from one shared constructor (`item_statements_expr`).

Seven `t/` files join the round-trip ratchet; `t/rakuast/rakuast-nonlvalue-assignment.t`
pins the read direction, the `EVAL` direction and the semantics.
