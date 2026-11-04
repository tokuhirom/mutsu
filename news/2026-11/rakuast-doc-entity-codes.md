# RakuAST: `E<...>` entity codes in Pod

A Pod paragraph with an entity code (`E<gt>`, `E<0x41>`, `E<amp;lt>`) was not
converted at all, so its whole `Doc::Block` was missing from `.AST`. It is now a
`Doc::Markup` of letter `E` with no atoms and a `meta` of one pair per
`;`-separated item, as rakudo does: a name is looked up in the HTML5 table
(`src/rakuast/html_entities.txt`, 2125 names, first code point only, checked
against rakudo for every name), a number (`123`, `0x41`, `0o101`, `0b1000001`)
is a code point, anything else keeps its spelling as `E<item>` (#14880).
