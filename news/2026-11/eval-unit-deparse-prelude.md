# An EVAL'd unit that calls `.DEPARSE` gets the deparser

The RakuAST deparser (rakudo's `RakuAST::Deparse`, ADR-14845) is prepended to a
program or module whose source text mentions `DEPARSE` or `RakuAST::Deparse`. A unit
handed to `EVAL` has no such text when it is a tree (`EVAL($ast)`), or when the word
is only in the EVAL'd string, so `.DEPARSE` died with `Could not find symbol '&run'
in 'GLOBAL::MUTSU_DEPARSE'`. `EVAL` now looks at the unit it runs and injects the
deparser (once per process, as for a program) when the unit calls `.DEPARSE` or names
the class. Three more `t/` files run as `EVAL(slurp($f).AST)`.
