# RakuAST: more barewords resolve to what they declare

Under the RakuAST round-trip frontend (`MUTSU_RAKUAST=1`) a bareword the converter cannot resolve is a refusal. Four families of declarations were invisible to the converter's scan and are now seen:

- a `&cb` parameter: a bare `cb` is an argument-less `Call::Name::WithoutParentheses`, as in rakudo;
- the sigilless elements of `my (\a, \b) := ...` and the parameter of `with X -> \y { ... }`: each is a `Term::Name`;
- `package GLOBAL::X::Y { class C }` declares the absolute name `X::Y::C`;
- an `EVAL` string now sees its caller's enum values (bare and qualified) and lexical classes (`my class P`), which the string alone cannot declare.

The ratchet in `ci/rakuast-frontend-passing.txt` grows by 15 files. The new test is `t/rakuast/rakuast-bareword-declared-scope.t`. Left over from the `BareWord` first-refusal cause (70+ files): `<value:sym<number>>` regex subrule names (rakudo keeps `sym<number>` as a name colonpair), a constant declared in an inner block and read from an `EVAL`, and names that only another module's `EXPORT` declares.
