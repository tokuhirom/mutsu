# Recognize the CORE `X` package in `when` matchers

The parser now treats the bare CORE exception package `X` as a complete matcher term. A `when X { ... }` block no longer raises a false block-gobbling syntax error, while undeclared barewords retain their diagnostic.
