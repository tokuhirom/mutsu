# README "Known Limitations" re-checked against a built binary

Two of the four bullets in the README's "Known Limitations" were out of date, so they were
re-measured with a fresh debug build against `raku`.

- **Multi-line feeds parse.** The README said feeds spanning multiple lines did not parse yet
  (and attached that to the `RakuAST` bullet, although the two are unrelated). Left-to-right
  (`==>`) and right-to-left (`<==`) chains split over several lines, ending in `my @r`, passing
  through `grep`/`map`/`sort`, and ending in a sub call and `say`, all print what `raku` prints.
  The README bullet now covers `RakuAST` only.
- **Exception types are nearly complete.** The README called the exception types "limited". Walking
  `X::` and one level of nested names, Rakudo defines 391 names and mutsu lacks 7 of them (it also
  defines 3 that Rakudo does not). Only three of the seven are exception types, all roles:
  `X::Await::Died`, `X::HyperRace::Died` and `X::Wrapper`. The other four are namespace packages
  (`X::Await`, `X::HyperRace`, `X::WhateverCode`, `X::WhateverCode::SmartMatch`). The bullet now
  names those three roles instead of implying a broad gap.

The other two bullets still hold: `say $undeclared` still runs and prints `(Any)` where `raku`
stops with `Variable '$undeclared' is not declared`, and `RakuAST` is still partial. Every code
example in the "What Works" section prints the output its comment shows.

The same stale feed and exception wording is still in `site/content/manual.en.js` and
`manual.ja.js` ("Known gaps"); `site/` runs the full CI, so it is left for its own change.
