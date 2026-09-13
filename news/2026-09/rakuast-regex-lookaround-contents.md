`RegexTree` now preserves the supported escaped digit class inside lookaround
assertions through `.AST` and `EVAL`, while keeping unsupported dynamic and
code-bearing forms on their existing paths.
