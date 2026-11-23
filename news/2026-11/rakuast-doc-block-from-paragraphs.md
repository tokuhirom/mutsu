# RakuAST::Doc::Block.from-paragraphs

`RakuAST::Doc::Block.from-paragraphs(:@paragraphs, ...)` now exists, ported
from rakudo 2026.11's `Fixups.rakumod`: the block's margin is stripped from
each string paragraph (a line with too little whitespace dies, as on rakudo),
and the paragraphs are interpreted by the block type: verbatim for `comment`,
`data` and `ignore`, a table for `table`, implicit code blocks for `item`,
`defn` (after its lemma), `pod` and the other container types, and formatting
codes otherwise. `RakuAST::Doc::Paragraph.from-string` now reads formatting
codes too (`"a B<b>"` is a `Paragraph` holding a `Markup`), and
`RakuAST::Doc::Markup.new` accepts a single `:atoms` / `:meta` item, as the
raku-doc examples use (#18981).
