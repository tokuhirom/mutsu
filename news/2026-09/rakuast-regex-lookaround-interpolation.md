# Scalar interpolation is retained inside lookaround assertions

`RegexTree` now preserves ordinary scalar interpolation inside supported
lookahead and lookbehind assertions. `.AST` exposes the nested
`RakuAST::Regex::Interpolation`, and execution reads the lexical value at
match time so a reused parser-created or constructed regex follows
reassignment. Code assertions, captures, subrules, and other runtime-valued
lookaround bodies remain explicit boundaries.
