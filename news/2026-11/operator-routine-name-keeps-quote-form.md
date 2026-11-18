# An operator routine's name keeps how its symbol was quoted in RakuAST

`sub postcircumfix:«<< >>»`, `sub infix:<< a >>` and `sub infix:«foo»` were normalised to the
`<...>` form before RakuAST saw them, so `.AST` and `.DEPARSE` lost the written delimiters and
spaces. The parser now records the written name next to the declaration's spans when it differs
from the canonical name, and the RakuAST view builds the symbol colonpair from that text
(a `quotewords` `QuotedString` with the raw text, as rakudo does). Lowering trims the symbol the
way the parser does, so `EVAL($ast)` still declares the same operator.
