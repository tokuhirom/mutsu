# Regex colonpairs retain explicit pointy signatures

RakuAST regex arguments such as
`<word(:expected(-> $candidate { $candidate eq $value }))>` now preserve the
parenthesized colonpair value and its explicit pointy-block signature. Source
and hand-built RakuAST regexes lower through the existing match-time evaluator,
so the parameter and captured lexical remain dynamic between matches.
