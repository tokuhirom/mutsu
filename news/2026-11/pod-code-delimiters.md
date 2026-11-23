# The RakuDoc reader takes every formatting-code shape rakudo does

Several formatting-code shapes made mutsu's RakuDoc reader decline the whole
paragraph and leave it as plain text, where rakudo reads them (#19091):

- an empty code: `B<>`, `B<<>>`, `B<<<>>>`;
- a run of four or more `<`: `B<<<<x>>>>`, closed by the same run of `>`;
- a `<<<` code holding a `|`, nested codes or guillemets;
- metadata with balanced brackets: `L<x|y<z>>`;
- an `E<...>` whose text names no entity: `E<lt<x>>`.

The reader now follows rakudo's rule. A code opens with `«` or any run of `<`,
and closes with `»` or the same number of `>`. Its content is read the same
way at every width: nested codes, metadata after `|`, and a lone `>` inside a
longer closer kept as text. An unterminated code still dies with rakudo's
message, now naming the full closer (`missing endtag '>>>>'`). The reader no
longer has a "decline" outcome besides that error. This change covers
`Paragraph.from-string`, `Doc::Block.from-paragraphs`, a source Pod block's
`.AST` and table cells.
