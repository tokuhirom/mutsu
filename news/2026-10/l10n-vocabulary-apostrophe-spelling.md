# An L10N keyword spelled with an apostrophe loads

`Str.AST("TLH")` (Klingon) died with "Slang activation NYI: grammar rule override 'block-loop'":
the parser re-spells a double-quoted token body that holds an apostrophe (`token block-loop {
"'eS" }`) as a single-quoted one with the apostrophe escaped (`'\'eS'`), and the vocabulary reader
refused any body with a backslash, so the token read as a production override. It now unescapes
that form. L10N::TLH's `t/01-basic.rakutest` prints "The answer" and passes like `raku`.
