# RakuAST: nested brackets in a bracketed quote

A balanced inner bracket pair inside a bracketed `q[...]` / `Q{...}` string is read as rakudo
reads it: `Q[ab [c] d]` is a `QuotedString` of `"ab ["`, a `QuotedString` of `"c"` and `"] d"`
(recursively), each piece with its range. The pieces are rebuilt from the source text only when
it reads back exactly as the literal (no escape, adverb or interpolation). A doubled bracket
delimiter (`Q[[a]]`) now puts its literal's range inside the inner pair (#15780).
