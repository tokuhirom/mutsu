# Regex codepoint escapes are case-exact under `:i`

`"A" ~~ /:i \x61/` matched in mutsu, while rakudo gives `Nil`. In rakudo a
codepoint escape is a `CharClass::Specified`, which names exact codepoints
that `:i` does not fold. This covers `\x61`, `\x[61,62]`, `\o[141]` and
`\c[NAME]`, and the negated `\X`, `\O` and `\C` forms. A quoted string holding
the same escape (`"\x[61,62]"`) is still folded, and so is a character class
(`<[\x61]>`).

Under `:i` the escape atoms are now wrapped in a case-exact group. A
comma-separated `\c[A, B]` list is now one atom, as `\x[…]` is, so a
quantifier applies to the whole run (#18777).
