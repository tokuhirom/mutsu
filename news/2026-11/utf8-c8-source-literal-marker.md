# A raw U+10FFFD in source text is a literal character, not a utf8-c8 synthetic

The last boundary of ADR-17800: a `U+10FFFD` written directly in a string
literal (`"􏿽xFF"`), in a heredoc or in a quoted regex term now enters the
`Str` escaped like every other literal marker, so `"􏿽xFF".chars` is 4 and
equals `"\x[10FFFD]xFF"`, as in rakudo. The escape moved into the parser's one
string-literal constructor (`literal_str`), which every piece of a quoted
literal goes through, so the `\x`, `\o` and `\c` escapes now push the raw
codepoint and nothing is escaped twice; the decoded text of a quoted regex term
is escaped the same way. ADR-17800's round-trip property test is in
`t/types/string/utf8-c8-literal-marker-roundtrip.t`: random strings mixing the
marker with `xFF`-like text survive `.ords`/`chrs`, `utf8`, `utf16`, `utf8-c8`
and `NFD`/`NFC`.
