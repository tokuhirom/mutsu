# Preserve capture numbering across alternation branches

Regex alternation now reserves `Nil` positional-capture slots for captures
that exist only in a wider branch. Captures following an alternation therefore
keep the number determined by the widest branch, for both `|` and `||` and for
the single-candidate matcher used by quantifiers.

The regression test covers both a short and a wide branch and verifies that a
following capture remains `$2`.
