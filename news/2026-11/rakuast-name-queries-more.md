# RakuAST: `Name` answers `simple-identifier`, `base-name`, `without-colonpairs` and `without-first-part`

`RakuAST::Name` now answers four more of the questions rakudo 2026.09 does
(`src/rakuast/name_queries.rs`): `simple-identifier` (the identifier of a one-part simple name, else
the Int `0`), `base-name` / `without-colonpairs` (a new `Name` over the same parts, colonpairs left
off) and `without-first-part` (a new `Name` without the first part; a one-part name gives the empty
name, which now spells as `""`). Matches `raku` on four names (one part, three parts, `GLOBAL::`,
two parts). `qualified-with(Name)` is not done: its one-argument row is never reached for a `Name`
instance (#16718). Test: `t/rakuast/rakuast-name-queries-more.t`; the two method-list pins list the
new names.
