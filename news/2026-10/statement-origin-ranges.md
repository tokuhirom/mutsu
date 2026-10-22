# Statement origins are Rakudo-compatible source ranges

`.origin` of a statement node used to answer a line number in both `.from` and `.to`. It is now Rakudo's range: `from` and
`to` are character (grapheme) offsets into the unit's source, `.source` is the unit's `RakuAST::Origin::Source`
(with `.orig`), and `.source.original-line($pos)` finds the line of an offset by binary search. The parser records the byte
range on the line marker it already emits before each statement, so nothing is re-derived from text. `Statement::If 11..46`,
statements sharing a line, multi-byte text, `\r\n`, combining marks, heredocs and pragmas match rakudo 2026.09
(`t/rakuast/rakuast-origin-ranges.t`). This is the first Stage 2 slice of ADR-10723 (§6.104); origins below the statement level
follow with their construct families.
