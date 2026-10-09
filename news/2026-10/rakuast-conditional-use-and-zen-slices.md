# RakuAST keeps conditional use and zen-slice adverbs

The RakuAST frontend now retains `use Module:if(EXPR)` conditions and explicit imports through its source round trip. The displayed `RakuAST::Statement::Use` keeps Rakudo's shape; hidden provenance restores the condition for the existing BEGIN-time use handling. Conversion does not evaluate the condition while parsing or analyzing source.

An empty subscript with a valid value adverb, such as `@a[]:k`, now has the empty `SemiList` Rakudo exposes. Lowering restores the parser's slice index before applying the adverb. An explicitly written value, such as `:k(True)`, stays a `ColonPair::Value` and keeps its parentheses. The existing `use if` and valid zen-slice adverb suites pass under the RakuAST frontend, and a focused read/EVAL test passes under Rakudo and mutsu.

This is an S10 slice of #7564.
