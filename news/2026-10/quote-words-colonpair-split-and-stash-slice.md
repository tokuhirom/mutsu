# `«&item:<errno>»` is two words, and `MY::«…»` slices the stash

Inside `«…»` and `<<…>>` a colonpair opens a word of its own: `«a:<b c>»` is `a`, `b`, `c`, `«x:<y>z»` is
`x`, `y`, `z` and `«a:b»` is `a` and the pair `:b`. mutsu kept `a:<b>` as one word. As a pseudo-package
subscript that matters: Unix::errno's test `ok MY::«&item:<errno>»:exists` is a slice over two names, and
the list it answers is truthy, whereas mutsu looked up one symbol `&item:<errno>` and answered `False`.
An unquoted, multi-word or colonpair-split `PKG::«…»` is now a slice built from the same quote-word list;
a quoted single symbol (`OUR::«'&infix:<@~~>'»`) is unchanged.
