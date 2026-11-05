# A character-class range may end (or start) at a surrogate

`<[\xA0..\xD8FF]>` made mutsu drop the regex's source tree (a Rust `char` cannot hold a
surrogate), so any unit containing it was refused by the RakuAST frontend ("literal Regex without
a declarator tree"), and the runtime class parser ignored the range, so it also matched wrongly.
No string holds a surrogate, so a range ending in one now ends at U+D7FF and a range starting at
one starts at U+E000, in both the source tree and the runtime class. The real
`HTML::Component::Encode` (and every module that uses it) now loads and matches `raku`.
