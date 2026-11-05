# RakuAST: the body of an anonymous `sub` / `method` has its ranges

The `Blockoid` of an anonymous routine now runs from its `{` to the end of the routine, and its
`StatementList` from the first statement to the end of the last (the `;` terminators included), or
sits right after the `{` when the body is empty, as in `raku` 2026.09 (`sub ($a) { 1 }`: body 9-14,
list 11-12). The parser records neither, so they are read off the unit's text
(`src/rakuast/anon_body_origins.rs`). A body whose statements carry no range of their own and that
contains a `#` gets no list range (a trailing comment would have to be excluded); the empty
signature node of `sub () { }` (rakudo 5-5) is still missing. Test:
`t/rakuast/rakuast-anon-sub-body-origins.t`.
