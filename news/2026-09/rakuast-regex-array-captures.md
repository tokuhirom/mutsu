# Regex array capture aliases retain their RakuAST boundary

Array-sigil regex capture aliases such as `@<word>=(a)` now retain their
`array` flag in the shared `RegexTree` and are exposed as
`RakuAST::Regex::NamedCapture`. Parser-created and constructed nodes lower
through the existing capture matcher, preserving list results for capturing
groups and their quantified iterations.

Hash aliases, subrules, and code-bearing regex nodes remain explicit
follow-up boundaries under ADR-0088.
