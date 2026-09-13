# Regex scalar interpolation retains its RakuAST boundary

The shared `RegexTree` now retains ordinary `$name` interpolation in regex
contents. `.AST` exposes it as `RakuAST::Regex::Interpolation` with the
corresponding `RakuAST::Var::Lexical`, and parser-created, constructed, and
token-declaration regexes lower plain scalar values to the existing match-time
matcher path. Regex-valued, collection-valued, and modifier-sensitive forms
retain the established parser path until their value-aware execution slices
are migrated.

Array/hash interpolation, code interpolation, named aliases, and subrules
remain explicit follow-up boundaries under ADR-0088.
