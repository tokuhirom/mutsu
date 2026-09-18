# Regex colonpairs retain multi-parameter explicit signatures

RakuAST regex arguments such as
`<word(:expected(-> $candidate, $other { ... }))>` now preserve every ordinary
positional parameter through source and hand-built regex lowering. The existing
match-time evaluator keeps the multi-argument callable and captured lexicals
dynamic; decorated signatures remain explicit follow-up boundaries.
