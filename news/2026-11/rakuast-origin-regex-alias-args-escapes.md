# RakuAST origins of regex alias arguments, regex arguments and escapes

`.AST` now places, as rakudo 2026.11 does:

- an aliased named assertion with arguments (`<alias=word("a")>`), with
  its name and argument list;
- a named assertion whose argument is a regex literal (`<v(/\d+/)>`). The
  argument scan now reads a `/.../` written in term position to its
  closing slash, so its backslashes and brackets no longer stop it;
- a character written as an escape in a character class (`<-[\x[22]]>`)
  and a range whose bounds are escapes (`<[\x[41]..\x[5A]]>`). The new
  `regex_escape_origins` reads `\x`, `\o` and `\c` codepoints (#15780).
