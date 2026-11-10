# Unspace before a declared postfix operator

`$tree\ data-root` (backslash-space, then a user-declared postfix operator, symbolic or a word)
was "Two terms in a row": the unspace was only consumed before the built-in postfix starters.
It now also glues a declared postfix onto the term, as rakudo does (found by Qwiratry's
`05-navigation-tree.rakutest`).
