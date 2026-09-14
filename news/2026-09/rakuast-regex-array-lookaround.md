# Array lookaround assertions retain their RakuAST shape

Direct `<?@name>` and `<!@name>` array lookaround assertions now retain
`RakuAST::Regex::Assertion::InterpolatedVar` in `.AST` and constructed RakuAST
trees. Matching continues to reread the live array through the existing
zero-width matcher, so reassignment after regex construction remains visible.
Named regex-argument array interpolation and other runtime-valued lookaround
bodies remain explicit boundaries.
