# RakuAST: `QuotedString.literal-value`

`RakuAST::QuotedString.literal-value` is implemented (#17922). A CHECK-time
analysis can now read a quote's literal argument, as on rakudo's RakuAST
frontend:

- A quote of literal segments is their text: `"abc"`, a processed `qq{a\tb}`,
  a dedented heredoc.
- An interpolated variable or block makes it `Nil`, and so does a run-time
  processor (`exec`).
- A word quote is its list of words: `qw/a b/` is `("a", "b")`, and `qw/a/`
  is `("a",)`.
- `val` turns each word into its allomorph, so `<a 42>` is
  `("a", IntStr.new(42, "42"))`. A `< >` quote of a single word is that word
  itself: `<42>` is an `IntStr`.
- `quotewords` keeps a quoted atom whole: `«a "b c" d»` is three words.
- `:stringify` gives the text before the word processors.

`maybe-compile-time-value` gives the same answer. It used to be `Nil` for any
quote with a processor.
