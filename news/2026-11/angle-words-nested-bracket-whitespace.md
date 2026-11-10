# Whitespace inside a nested `<...>` pair of a quote-word list

`<& < > '>` is `("&", "<>", "'")` in raku: whitespace right after a nested `<`
and right before a nested `>` is dropped. mutsu split it into four words, which
made Pod::To::HTML's `t/130-links.t` run one more subtest case than raku.
