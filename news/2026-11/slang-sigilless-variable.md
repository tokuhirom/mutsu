# A slang's `sigilless-variable` token: `my name = 42` and `my 👍 = 42`

Slang::Nogil (`<.ident>+ | <.:So>`) and Slang::Emoji (`<.:So>`) override the
`sigilless-variable` token so that `my NAME` declares a variable with no sigil.
The override now sets a parser mode: after `my` (and after a type constraint,
`my Int n = 7`) an identifier, or one Symbol-other character, names a variable
exactly as `my \NAME` does, and the name reads back as a term. An identifier
that opens a declaration (`class`, `sub`, `method`, ...) stays a keyword.

Along the way, the bareword before `=>` is quoted only when it is an
identifier. A name that is not one (the emoji above, a `term:<☃>`) is a term, so
`my %h = 👍 => 666` keys on the variable's value, as rakudo does.

Slang::Emoji (4 of 4) and Slang::Nogil (27 of 27) match rakudo.
