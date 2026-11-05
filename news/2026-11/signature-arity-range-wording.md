# Arity errors state a range, and concreteness errors word-wrap

"Too few/many positionals passed" now words the arity like rakudo: `1 or 2 arguments`,
`1 to 4 arguments`, and `at least 1 arguments but got only 0` when a slurpy lifts the upper
bound (it said `expected 4 arguments` for a signature with three optionals, and `expected 1
argument` for `($x, *@r)`). A method counts its invocant in both bounds. The wording lives in
`runtime/types/arity_message.rs`.

`X::Parameter::InvalidConcreteness` for a named parameter is wrapped at 72 columns by the same
`naive-word-wrapper` as the invocant form instead of at fixed break points. Both found by the
raku-doc differential (`Language/signatures.rakudoc:299`, `:329`, `:703`).
