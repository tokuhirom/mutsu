# Sequential scalar interpolation is retained in lookarounds

`RegexTree` now distinguishes sequential alternation and preserves the
`sequential => True` interpolation node for scalar variables after `||` inside
supported lookahead and lookbehind assertions. The existing matcher retains
branch priority and reads the scalar value at match time. Array interpolation,
code interpolation, and other runtime-valued branches remain explicit
boundaries.
