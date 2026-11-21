# `.origin` of declarator docs

The `Doc::Declarator` that `.WHY` gives a documented declaration (a sub, a
variable, a parameter, an attribute) now carries the range rakudo 2026.11
gives it. The range runs from the first `#|` / `#=` comment to the end of the
last one's text. The parser records where each declarator comment ends, and
`DeclDoc` keeps the span of the comments it collects.

Declarator comments also no longer stop the statement lists around them from
being placed. Before this, a `#|` before the first statement of a unit or
block, or a `#=` after the last one, left that `StatementList` without a range;
they are now trivia like any other comment, as on rakudo (#15780).
