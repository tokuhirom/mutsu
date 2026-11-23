# A term key's package-chain probe skips the alias cascade

The type-check memo's hit path asks, for every bareword type name it checks
(`Bool`, `Str`, ...), whether a constant or sigilless term of that name shadows
the type here. That question walks the enclosing package chain, and for each
package candidate (`Pkg::\Bool`) it ran the whole
`get_env_with_main_alias_sym` cascade: process dynamics, atomic lanes, unit
lexicals, `our` mirrors, unmerged package variables, twigil aliases. None of
those can hold a term key, so the package chain now asks only the direct env
probe for one. A debug assertion checks that the full cascade agrees.

The AttrX::Mooish t/090 probe drops from 4,621,591 to 4,494,338 Ir per round
(−2.75%, callgrind slope, profiling build) (#18274).
