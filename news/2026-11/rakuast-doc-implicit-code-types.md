# RakuAST Pod: implicit code blocks only where rakudo makes them

Mutsu turned every indented paragraph of a Pod block into an
`implicit-code` block. Rakudo's `from-paragraphs` does that only in `cell`,
`defn`, `item`, `nested`, `pod`, `rakudoc` and `section`; in a custom block
(`=begin Foo`), a heading or a `=for Podcast` paragraph the indented text is
kept as written. As in rakudo's `!interpret-implicit-code-blocks`, a paragraph
right after a nested block and indented like that block's first paragraph is
text, not code. An implicit code block inside an indented block (`    =begin
item`) is now read, and an indented `=end code` inside a code block is part of
its text.

With this, the `.AST` of all 27 `roast/S26-documentation` files matches
rakudo 2026.11 node for node (#14880). The parser's own Pod skipping still
ends a delimited block at an indented `=end`, which changes what executes;
that is #18369.
