# Attribute statement modifiers round-trip through RakuAST

Postfix condition modifiers on class attributes now preserve their source declaration through RakuAST. The attribute remains installed regardless of the condition, while the condition still runs once during class composition. The parser and RakuAST lowerer share the same expansion path.
