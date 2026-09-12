# Regex values retain their source trees

Static parser-created regex values now keep their shared `RegexTree` through
the compiler. The ordinary single-match smartmatch path lowers that tree
directly to the existing `RegexPattern` matcher, while unsupported and
dynamic values continue through the established parser fallback.

This covers both plain and static adverb-bearing values without changing the
`Regex` view or the Parser -> Compiler -> VM matcher path. String-only entry
points, captures, subrules, interpolation, and runtime adverb arguments remain
open under ADR-0088.
