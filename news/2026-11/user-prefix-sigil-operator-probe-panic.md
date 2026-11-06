# A user `prefix:<%>` no longer panics the parser

An imported `prefix:<%>` (Math::Matrix exports one) made the parser probe whether a
`%` starts an ordinary hash variable while it still held a borrow on the lexical
scope table; the probe records anonymous-state names in that same table, so loading
`Chemistry::Stoichiometry` panicked with `RefCell already borrowed`. The probe now
runs after the table is released and discards the names it minted. All four
Chemistry::Stoichiometry test files print what `raku` prints.
