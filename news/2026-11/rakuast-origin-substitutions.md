# RakuAST origins of substitutions, and `s[...] = 'literal'`

`.AST` now places the parts of a substitution, as rakudo 2026.11 does:
the `:g` / `:i` adverbs, the pattern (laid out by the regex walk), and the
replacement, which is either the string between the second and third
delimiters (`s/a/b/`, `S/a/$0/`, `ss/a b/c d/`) or the operator and
expression after a bracketed pattern (`s[\d] = 5`, `s[\d] += 5`).
`visit-children` now visits a substitution's pattern before its operator.

`s[...] = LITERAL` no longer takes a shortcut that kept a lone literal as a
replacement *string*. That shortcut lost the `Assignment` and the
expression from `.AST`, and interpolated a single-quoted literal:
`$s ~~ s{a} = q{$x}` put the value of `$x` in. The replacement is now always
the expression, evaluated per match, as any other `s[...] = EXPR`. The same
holds for `S[...] = ...`, and `ss[...] = EXPR` now accepts any expression.
