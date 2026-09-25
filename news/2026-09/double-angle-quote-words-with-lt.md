# A `<` inside `<< >>` is word text

`<< < ≤ <= >>` is a three-word list in Raku, `("<", "≤", "<=")`, and `<< a<b >>` is the
single word `"a<b"`. mutsu failed to parse both with `Confused. expected statement`
([#9325](https://github.com/tokuhirom/mutsu/issues/9325)). That broke the same line in
the autocomplete tests of **Jupyter::Kernel** 1.0.4 and **Jupyter::Chatbook** 0.3.9:

```raku
is $c.complete('<'), (0, 1, << < ≤ <= >>), 'less than';
```

The cause was `find_quote_word_close` in `src/parser/primary/container/angle_words.rs`,
which looks for the closing `>>`. It counted every unquoted `<` as a nested opener and
matched it with the next `>`, so a word containing `<` without a matching `>` pushed the
real closer out of reach. Rakudo gives `<` no bracket meaning inside `<< >>`:
`<< <a b> c >>` is `("<a", "b>", "c")`, not a quote-protected pair. The counter is gone.

Subscripted interpolation still ends where it should. A `>>` that is immediately followed
by another `>` was already skipped as the closer, so the first `>` of `<<x %h<b>>>` still
closes the subscript, and the rest of the `>>>` closes the list.

Pinned by `t/lang/quoting/double-angle-quote-word-lt.t`.
