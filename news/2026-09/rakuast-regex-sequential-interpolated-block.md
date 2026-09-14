# Sequential interpolated regex blocks retain their RakuAST shape

An interpolated regex code block after `||`, such as `foo || <{ "bar" }>`, now
retains `RakuAST::Regex::Assertion::InterpolatedBlock` with
`sequential => True` under `RakuAST::Regex::SequentialAlternation`. Ordinary
interpolated blocks remain non-sequential.

The existing runtime parser and matcher continue to handle the code-bearing
branch, including match-time lexical reassignment and sequential branch
priority.
