# Static regex trees lower to the execution matcher

The static `RegexTree` subset used by RakuAST now lowers directly to mutsu's
existing `RegexPattern` matcher plan for literal sequences, quotes, groups,
alternation, digit classes, simple quantifiers, and the ratchet/case policies.
Patterns outside that subset continue through the established structural parser
until their dynamic semantics can be represented safely.

The Parser -> Compiler -> VM path and existing matcher remain unchanged. The
parser-produced tree still needs to be transported through regex values before
the full string-to-plan boundary can be removed; captures, subrules,
interpolation, and runtime adverb arguments remain open under ADR-0088.
