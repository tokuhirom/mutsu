RakuAST conversion now preserves multiple inline declarations inside one
comma list. Conversion exposes each declaration as its own measured
`ApplyListInfix` operand, and lowering regroups adjacent declarations to retain
their shared scope. This keeps sibling declarations intact when assigning
through a list subscript.
