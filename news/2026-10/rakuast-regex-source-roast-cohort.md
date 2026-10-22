# RakuAST retains two more regex source forms

Callable aliases such as `<wc=&wc>` now keep an `Assertion::Alias` with a
callable child in the regex source tree. A bare scalar interpolation before a
quote, such as `$'x'`, also keeps its source tree and round-trips as an
anonymous state scalar followed by the quote. Both forms lower back to the
existing regex execution path.

The focused RakuAST regression checks the node shapes and EVAL semantics.
Four formerly refused roast files pass under `MUTSU_RAKUAST=1`:
`S05-interpolation/lexicals.t`, `S05-match/capturing-contexts.t`,
`S05-match/raku.t`, and `S05-metasyntax/unknown.t`.
