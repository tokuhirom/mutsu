# `\c[0x0730]` names a codepoint

rakudo reads a radix-prefixed number inside `\c[...]` (`0x`, `0o`, `0b`) as a
codepoint, in a string, a regex atom and a character class. mutsu looked the
text up as a character name, found nothing and, for a regex literal, produced no
source tree, so `MUTSU_RAKUAST` refused the whole unit ("literal Regex without
a declarator tree"). The shared name lookup (`lookup_unicode_char_by_name`) now
accepts the prefixed forms. Found with Lang::Transliterate, whose
`t/01-basic.t` now matches raku.
