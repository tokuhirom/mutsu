# `require Module <KEY>` imports enum keys, and `\0` matches NUL in a regex

Two gaps surfaced by Crypt::SodiumPasswordHash's `t/030-hash.t`, which now
produces the same TAP under mutsu as under rakudo:

- `require Crypt::SodiumPasswordHash <&sodium-hash &sodium-verify ARGON2I13 ARGON2ID13>`
  died with `X::Import::MissingSymbols: ARGON2I13 ARGON2ID13`. The runtime
  `require` import list never consulted the module's export table for enum
  keys, which `use` installs into the enum-key namespace. `require` now takes
  an exported enum key from the same table and installs it the same way.
- `\0` outside a character class was not recognized as the NUL character, so
  `.subst(/\0+$/, '')` left the NUL padding of a C buffer in place and the
  next NativeCall rejected the string. `\0` is now a NUL literal in a regex
  atom, as it already was inside `<[...]>`.
