# Trailing `-` before `]` in a character class, and `~$obj` on a Date subclass

Two fixes found running `TOML::Thumb`:

- `<[ A..Z a..z 0..9 _ - ]>`: a `-` followed only by whitespace and the closing `]` is a literal
  hyphen. The regex source-tree builder treated it as an interior range operator and gave up, so the
  whole `token` had no source tree and the module died with "RakuAST: `.AST` does not yet support this
  construct: regex declaration without a source tree".
- Prefix `~` on a subclass of `Date` / `DateTime` called the built-in `Stringy` and ignored the
  subclass's own `Str` (`class DateTime::Local is DateTime { method Str { callsame.subst: 'Z' } }`).
  `Mu.Stringy` is `self.Str`, so a user `Str` now answers `~$obj`.

`TOML::Thumb`'s `t/invalid.t` and `t/valid.t` now match Rakudo (same TODO failures only).
