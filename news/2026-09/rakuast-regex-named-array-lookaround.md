# Named lookaround array interpolation retains its RakuAST shape

Named regex-argument lookarounds such as `<?before @name>` and
`<!before @name>` now retain `RakuAST::Regex::Interpolation` in `.AST` and
constructed RakuAST trees. Matching rereads the live array through the
established matcher, so reassignment after regex construction remains visible.
Code assertions and other runtime-valued lookaround bodies remain explicit
boundaries.
