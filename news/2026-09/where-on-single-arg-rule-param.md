# `where` on a single-argument-rule parameter (`+bar where ...`) parses and is enforced

`WhereList`'s `t/01-basic.t` declares
`sub foo (+bar where all-items any Str|Nil|Int:D, * === Any) { 42 }` and died
at that line with this #7988 cluster's generic
`Confused. expected statement: expected ')'`. The sigilless single-argument-rule
slurpy (`+bar`) had its own branch in the parameter parser that accepted traits
and a default but not a `where` clause.

That branch now takes an optional `where` constraint, through a small
`parse_optional_where` helper shared with the `\bar` branch. Pinning it down
showed that the constraint was not *enforced* on either spelling: `+bar where`
and `+@bar where` bound any argument list, while `*@bar where` already checked
it. Both single-argument-rule binding paths (the lazy one included) now run the
constraint against the collected list, which also makes such a `where` a real
multi-dispatch discriminator. A sigilless parameter is now named bare in a
binding error (`parameter 'bar'`, `parameter 'v'`), as rakudo does, instead of
gaining a `$`.

Pinned by `t/routines/signature/where-plus-sigilless-param.t`. WhereList's test
now parses and runs to line 16, where it stops on a separate closure-capture
bug (a closure invoked through `~~` sees a stale `@`-sigiled capture from an
earlier closure of the same routine), filed as #9429.
