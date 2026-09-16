# Escaped quotes no longer terminate a regex group scanner early

The regex group scanner now treats a backslash-escaped quote as part of the
quoted literal instead of closing that literal. This preserves following
character classes in bracketed alternations such as:

```raku
/ ^ ['\'' | '"'] <-['"]>+ '\'' | '"' $ /
```

The bug blocked `FunctionalParsers::EBNF::Actions::MermaidJS::Common` from
loading. `FunctionalParsers` 0.1.10 consequently moved from `blocked_load` to
`partial`: 6 of 18 Rakudo-baseline test files now pass under mutsu. The
remaining grammar/action behavior is tracked in #8526.
