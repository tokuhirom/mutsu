# Rakudo::Internals.NORMALIZE_ENCODING

`Rakudo::Internals.NORMALIZE_ENCODING($name)` answers rakudo's canonical
spelling of an encoding name (`UTF-8` -> `utf8`, `latin-1` -> `iso-8859-1`,
`utf-16-le` -> `utf16le`, `windows1252` -> `windows-1252`; any other name
lowercased; an undefined argument is `utf8`), identical to rakudo for every
name in mutsu's encoding table in any letter case. IO::Socket::Async::SSL's
`Supply(:enc(...))` calls it and died with "No such method".
