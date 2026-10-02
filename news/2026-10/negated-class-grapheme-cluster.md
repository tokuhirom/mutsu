# A negated character class matches a grapheme with a combining mark

A character class tests a whole grapheme, but mutsu evaluated it against the
grapheme's base character. So `"x\x[301]" ~~ /<-[a..z]>/` was False: `x` is in
the excluded range, even though the one-grapheme `x́` is not `x`.

Following rakudo, an enumerated character or range in a class now never
matches a grapheme of several codepoints. The class's other items (`\w`,
`\s`, `<:L>`, ...) still test its base character. So `<-[a..z]>` matches
`x́`, `<[\w]>` still matches it, and `<[a..z\s]>` does not. The rule applies to
plain classes and to class arithmetic (`<-[\w] + [x]>`), and the scan
prefilter now always offers a position that starts such a cluster (#10748).
