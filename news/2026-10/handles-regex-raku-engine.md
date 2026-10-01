# `handles /regex/` matches with the Raku regex engine

`handles /<regex>/` delegation used to cut the pattern out at the next `/` and test method names with the PCRE-syntax `fancy-regex` crate, so Raku-only constructs (`<[a..z]>`, `<alpha>`, quoted literals, insignificant whitespace) never matched. The parser now scans the pattern with the regex-literal scanner (escaped `\/` works) and dispatch matches the method name with mutsu's own regex engine, whose compiled patterns are cached. The `fancy-regex` dependency is removed. Closes #10436.
