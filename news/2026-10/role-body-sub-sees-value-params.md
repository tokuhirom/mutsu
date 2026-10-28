# A `sub` in a parametric role body sees the composition's value parameters

`role BitEnum[::E, Str:D :$prefix = '', Bool :$lc] { sub lookup($s) { ... $prefix ... } }`
composed with `:prefix<SQ_>` read `$prefix` as undefined inside `lookup`: the
sub was registered once, at the role's declaration, and the per-composition
body run kept that copy (secluded in the compunit-private table). Each
parametric composition that has value parameters now re-declares the body's
plain `sub`s in the composing class with the parameters bound, the way a body
`my` lexical already was (#13957), and drops the parameter-less copy. DB::Xoos'
`t/03-sqlite.t`, `t/04-relations.t` and `t/06-compound-keys.t` (through
BitEnum) now pass as they do on rakudo. Fixes #15100.
