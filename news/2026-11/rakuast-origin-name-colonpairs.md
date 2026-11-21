# `.origin` of a named colonpair in a routine's name

A routine name that ends in a named colonpair, such as `method comment:sym<#>`
or `method m:sym<a b>`, now places that colonpair as rakudo 2026.11 does:
- the `ColonPair::Value` runs over `:sym<#>`;
- its `QuotedString` runs over `<#>`;
- its `StrLiteral` runs over the text between the brackets.

Before, only an operator's bare `<...>` (`infix:<foo>`) was placed. The
name-quote placement moved out of `routine_origins.rs` into
`name_quote_origins.rs`, and each part is now checked against the source text
(#15780).
