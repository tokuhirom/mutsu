RakuAST conversion now preserves multiple inline declarations inside one
comma list. Conversion exposes each declaration as its own measured
`ApplyListInfix` operand. Lowering restores their shared declaration block for
subscript stores, while preserving individual declaration terms as
metaassignment targets. This keeps sibling declarations intact in both forms.
