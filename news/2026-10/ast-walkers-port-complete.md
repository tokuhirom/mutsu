# Every read-only AST analysis is on the typed visitor

Issue #10468 is closed. It ported the hand-rolled recursive `Stmt`/`Expr` walkers onto the typed
visitor (ADR-0137) and, for the rewriting passes, onto `VisitMut` (ADR-10499). These walkers had a
`_ =>` arm, so each one silently skipped every AST variant it did not list.

The last read-only analysis was the RakuAST converter's declared-name scan. Before the port it
stopped at the constructs it listed. As a result, `if 1 { class E { } }; E.new` could not be
converted (`.AST` reported `BareWord("E")` as unsupported). Now a type or constant declared in an
`if` or loop body, a closure or a `do` block renders as `Type::Simple` / `Term::Name`, as rakudo
does.

`scripts/ast-walkers-baseline.txt` went from 265 walkers to 106. Every remaining row has a note
saying why the walker stays hand-rolled:

- code generation (`compile_*`, TRIR, RakuAST conversion, declaration plans);
- single-path spines (lvalue chains, statement tails);
- parser lowerings and recursive-descent parse functions;
- shape matches, constant evaluators and source renderers;
- the walks that visit only some positions by design (sink-context propagation, curry operand
  positions).

Along the way, duplicated non-visitor logic was folded into one implementation each:

- the statement-ending-brace test, which fixed three "Two terms in a row" parse bugs;
- `use lib` argument decoding;
- operator-chain flattening;
- the variable-key atom and lvalue/index spines;
- the block-tail statement choice;
- the `SyntheticBlock` scope iteration.

Each port checked the positions it newly reached against rakudo and pinned them in tests.
