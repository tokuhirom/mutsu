# Statement modifiers are a typed parser node

Statement modifiers move to ADR-10723 Stage 2 (#14938). The parser used to
desugar each modifier while it parsed:

- `S if C` became an `If`, and `S unless C` an `If` over `!C`.
- `S with T` became `given T { if .defined { S } }`.
- `S when M` became a `given $_` around a `When`.
- `S for L` became a `For`.

It now builds one `ast::parsed::Modified` node instead. The node holds the
statement as written, its condition modifier and its loop modifier, with the
source range of the modified statement and of each modifier.
`rakuast::modifier_lower` holds the desugaring, and only it. An executing
parse lowers the node where it is built, and a `.AST` parse renders it. A
hand-built `Statement::Expression` with a `condition-modifier` or
`loop-modifier` is read into the same node and lowered by the same function.

The duplicate implementations are gone:

- `.AST` no longer reverse-engineers the desugared shapes back into
  `StatementModifier::*` nodes (the modifier branches of the `If` / `While`
  / `For` / `Given` arms, `when_modifier_parts`, `with_modifier_node`, the
  routine-declaration-plus-`given` arm).
- `rakuast::lower` no longer keeps its own copy of the desugaring.
- `rakuast::modifier_origins` no longer guesses ranges by re-reading the
  source.
- The declaration-modifier source records that only the conversion read are
  no longer built.

The ranges now also come out right where the guessing gave up. With two
modifiers (`say 1 if $x for 1, 2`), each modifier has its own range. With a
comment before the keyword, the modified call's range includes the comment,
as in rakudo 2026.09. `t/rakuast/rakuast-statement-modifier-node.t` pins
these ranges and checks that the lowered trees run like the parsed program.
