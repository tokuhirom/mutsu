# Parse capture markers before alternation

Regex alternation splitting now treats `<(` and `)>` as capture markers rather than grouping delimiters. A marker in a non-final branch no longer hides the following alternative.
