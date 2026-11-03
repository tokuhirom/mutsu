# RakuAST: `Doc::Markup`, `Doc::Paragraph` and formatting codes in Pod

`RakuAST::Doc::Markup.new(letter, opener, closer, atoms, meta)` (the delimiters default to `<` and
`>`) and `RakuAST::Doc::Paragraph.new(|@atoms)` are modelled (constructors, `.raku`, accessors;
the accessor rows were regenerated). A paragraph of a Pod block with formatting codes (`B<bold>`,
nested codes, `C<<a b>>`, `B«x»`, a code over several lines) is now a `Doc::Paragraph` of strings
and `Doc::Markup` nodes, as rakudo's. Codes with metadata (`X<a|b>`, `L<text|url>`), `E<...>`,
a letter that follows a word character and any ambiguous bracket inside a code leave their block
out of the tree, as before. Part of #14880.
