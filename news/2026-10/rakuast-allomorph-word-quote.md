# RakuAST: allomorph literals `<42>` across the boundary

After the indexed-bind slice, 64 `t/` files were refused at an allomorph
literal: `<42>`, `<1.5>`, `<1e3>`, or the number-shaped words of a longer
quote-word list. mutsu's parser evaluates `<42>` to the allomorph, an IntStr
held as `Mixin(Int(42), {Str => "42"})`, and the converter had no node for a
mixin value.

Measured on rakudo 2026.09, `<42>` is a
`QuotedString(processors => <words val>, segments => (StrLiteral("42"),))`.
The allomorph keeps the word it was read from, so:

- the converter renders that word as rakudo's node;
- the lowerer hands the text back to `parser::angle_words_expr`, the same
  quote-word evaluation the parser itself runs.

That function is now split out of the `<…>` parser.

The converter checks that reading the word back yields this one allomorph,
and declines otherwise:

- a word with whitespace or a quote-word escape;
- the content of a numeric literal term (`<1/2>` is a `Rat`, not a `RatStr`);
- a mixin that is not an allomorph.

Lowering a `QuotedString` now refuses a processor list other than
`<words val>` rather than silently ignoring it.
