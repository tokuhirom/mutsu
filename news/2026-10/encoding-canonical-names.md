# Canonical UTF encoding names

`Encoding::Registry.find` now reports `utf8`, `utf16`, `utf16le` and
`utf16be`, matching Rakudo. Hyphenated spellings remain accepted aliases.
Encoding and wide-buffer decoding also recognize the canonical spellings,
preserving byte order when the registry resolves an alias.
