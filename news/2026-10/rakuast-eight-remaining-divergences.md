Eight more language tests now pass through the RakuAST frontend. Nested word
quotes preserve their delimiter and child quote structure, and comparison
chains resume after a regex smartmatch. Enum values, post-declared listops,
native defaults, frame-state NQP ops, bare-block placeholders, and overloaded
`<=>` retain their ordinary execution behavior after round-tripping.
