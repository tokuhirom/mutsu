# A `has` declaration's postfix modifier is the generic modified-statement node

`has $.x if COND`, `has $.x for LIST` and a modifier chain on an attribute
declaration now parse to the same `ParsedStmt::Modified` node every other
statement gets. The parser no longer wraps the declaration and an empty
`SyntheticBlock` placeholder, and the dedicated `ParsedStmt::AttributeModifier`
node, its `attribute_modifier_source` operand parser and its view/lowering arms
are gone. `rakuast::modifier_lower` is the single place that keeps the
declaration and evaluates the modifier operands once (a loop modifier over an
attribute no longer needs a parser-built block). `.AST` of `has $.x for 1, 2`
now shows the `loop-modifier`, and `synthetic-block-sites` drops 27 -> 23.
