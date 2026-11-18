# An infix declared `is looser<,>` takes the whole comma list

`sub infix:<graf>($a, +@b) is looser<,>` declares an operator one step looser
than the comma operator, so `1 graf 2, 3` is `graf(1; (2, 3))`. mutsu had no
parse-time level for `,`, so the trait was silently dropped and the operator
stayed at the additive default: `1 graf 2, 3` parsed as `(1 graf 2), 3`.

The parser's precedence map now has a comma level between the list-infix
level and `||`; `is looser<,>` lands at the list-infix level, whose right
operand is the comma list, and `is equiv<,>` / `is tighter<,>` resolve too.
This holds for a local declaration and for an imported operator (#18094).
