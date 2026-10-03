# Role parameterization selects a candidate eagerly

`Role[args]` now resolves the parametric role variant when the type is
parameterized, so arguments that no candidate's signature accepts throw
`X::Role::Parametric::NoSuchCandidate` at the subscript, as in Rakudo, instead
of deferring until the role is composed. Found through the `Parameterizable`
distribution, whose `.^parameterize` relies on `try obj.MIXIN(|@pos)` failing.
