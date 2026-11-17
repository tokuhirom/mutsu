# RakuAST: `Name.canonicalize` keeps a name's colonpairs

`Name.canonicalize` of an extended name such as `today:foo<g h>` answered the
bare identifier, `today`. The `.DEPARSE` of a declaration reads
`desigilname.canonicalize`, so `my $three:foo = 3` deparsed as
`my $three = 3`. Rakudo keeps the colonpairs, canonicalized (`:k`, `:!k`,
`:k<v>`).

`canonicalize` on a name that carries colonpairs now answers the canonical
spelling. It uses the same rule that lowers a declared name's colonpairs
back into the variable name. Found through the `.DEPARSE` corpus
(`S03-operators/ternary.t`, #14880).
