# Parenthesized handles word lists

Attribute delegation accepts word lists inside parenthesized `handles` clauses,
including `handles(<keys Bool>)` and mixtures of quoted names and word lists.
Both spellings use the ordinary quote-word parser and resolve static method
names during parsing. Nested list terms share a bounded visitor with RakuAST
lowering, preserving the written handles term without adding runtime evaluation.
