# ADR-15760 (NameScope) is revised and Accepted

The maintainer answered its open questions: classification is in source order with no whole-unit pre-scan and only
routine names late-bound, slice 0 (`NameScope` as an explicit parameter, no behaviour change) comes first, and the
type lives in `src/scope.rs`. The ADR now states that rakudo hoists routines only (measured: `say A; class A {}`,
`say X; constant X = 3` and `say x; my \x = 1` differ from mutsu, tracked in #15791), that lowering and every
`declare_*` happen at a statement commit point because of the parser's backtracking, that inside the parser
`NameScope` is the type of the existing thread-local scope state, the prerequisite layering slice, the constructs
`finalize` still converts, the cache format bump, and the slice order.
