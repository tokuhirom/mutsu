# RakuAST preserves unresolved qualified names

Parseable unresolved qualified names such as `A::B` now round-trip as
`RakuAST::Call::Name`, including when followed by a method call. Evaluation
still resolves them through the normal named-call path, so a missing lookup
remains catchable by `try`.
