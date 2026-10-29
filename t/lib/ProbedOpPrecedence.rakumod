module ProbedOpPrecedence::Ops {
    our only infix:<gg>(Mu $p is raw, +@v is raw) is looser<...> { "$p|" ~ @v.elems }
}

my package EXPORT {
    package MANDATORY {
        OUR.WHO.BIND-KEY: .key, .value
            for ProbedOpPrecedence::Ops::;
    }
}
