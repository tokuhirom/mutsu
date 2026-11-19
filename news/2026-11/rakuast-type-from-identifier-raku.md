# RakuAST: `.raku` prints a plain type name as `from-identifier`

`Type::Simple`, `Type::Setting` and `Type::Capture` over a plain name print as
`RakuAST::Type::Simple.from-identifier("Int")` (or `.from-identifier-parts("A","B")`) in
rakudo 2026.11, instead of `.new(` around a `Name`. mutsu's `.raku`/`.gist` now prints the
same, which was the largest single difference class in the `.AST` text corpus (about 900
of 4300 sampled statements). The tests that pinned the old spelling are updated; they
pass under rakudo 2026.11 too.

A name with colonpairs (`class A:ver<1.0>`, `sub infix:<+>`) likewise prints as
`Name.new(Name::Part::Simple.new("A"), colonpairs => (...))`, and the `.AST` text golden
(`ci/ast-text-corpus/rakudo.tsv`) is re-recorded on rakudo 2026.11 for the same files. Four
statements that matched the older rakudo text but differ from 2026.11's only in the
`Name::Part::EmptyEdge` rename (#18241) leave `identical.tsv` until that is fixed; the
re-recorded golden lets 852 more statements join it.
