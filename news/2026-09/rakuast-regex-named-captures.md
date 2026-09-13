# Regex named captures retain their RakuAST boundary

Ordinary scalar named captures such as `$<word> = a` now remain in the shared
`RegexTree` and are exposed as `RakuAST::Regex::NamedCapture`. Parser-created
and constructed nodes lower to the existing named-capture matcher, including
the whole-run semantics of a scalar alias around a quantified atom.

Subrule aliases, array/hash aliases, and code-bearing regex nodes remain
explicit follow-up boundaries under ADR-0088.
