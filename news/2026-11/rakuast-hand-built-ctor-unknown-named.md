# RakuAST constructors refuse an unknown named argument everywhere

`RakuAST::ApplyInfix.new(..., bogus => 1)` and every other RakuAST constructor that is not
schema-driven (`Block`, `Statement::Expression`, `ColonPair::Value`, routines, parameters, ...)
accepted a named argument the class does not take and ignored it. Rakudo's bind dies with
`Unexpected named argument 'bogus' passed`; mutsu now does the same.

The accepted names are not hand-kept: `scripts/rakuast-ctor-names.raku` reads the named parameters of
every `new` candidate of every RakuAST class from rakudo 2026.09 into `src/rakuast/ctor_names.tsv`
(199 classes whose `new` has no slurpy hash), and one check in `construct_values` applies it before
any constructor builds anything. A class without a row (`Mu.new`) still accepts any name, and the
documented model extension `Regex::NamedCapture`'s `hash` stays accepted.

Test: `t/rakuast/rakuast-ctor-unknown-named-hand-built.t`, identical under `raku`.
