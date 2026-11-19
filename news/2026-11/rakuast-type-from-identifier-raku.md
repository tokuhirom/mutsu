# RakuAST: `.raku` prints a plain type name as `from-identifier`

`Type::Simple`, `Type::Setting` and `Type::Capture` over a plain name print as
`RakuAST::Type::Simple.from-identifier("Int")` (or `.from-identifier-parts("A","B")`) in
rakudo 2026.11, instead of `.new(` around a `Name`. mutsu's `.raku`/`.gist` now prints the
same, which was the largest single difference class in the `.AST` text corpus (about 900
of 4300 sampled statements). The tests that pinned the old spelling are updated; they
pass under rakudo 2026.11 too.
