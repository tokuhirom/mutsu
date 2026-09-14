# Callable regex interpolation retains its argument tree

`<&name("value")>` and `<&name: "value">` now cross mutsu's RakuAST regex
boundary as `RakuAST::Regex::Assertion::Callable` with a non-empty
`RakuAST::ArgList`, matching Rakudo's AST shape. The parser preserves both the
parsed expressions and their source spelling, while colon syntax normalizes to
the same model as parentheses.

Hand-built `ArgList.new(...)` and callable assertion nodes can also be lowered
and evaluated as regexes. Matching continues through mutsu's existing runtime
regex parser, with unsupported hand-built argument expressions rejected at the
lowering boundary rather than silently mis-rendered.

The focused regression is `t/regex/regex-tree-callable.t`; this continues the
work tracked by [#8033](https://github.com/tokuhirom/mutsu/issues/8033).
