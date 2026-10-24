# `$n++` after `use Red`: an imported `postfix:<++>` no longer swallows Int increments

Once a program imported a user `postfix:<++>`/`prefix:<++>` (Red exports its AST-column ones),
the parser turned every `$n++` into a call so the runtime could rank the native operator against
the user's candidates. When the candidates arrived through an `EXPORT` map they were bound as a
dispatcher `Sub` under the operator's name rather than as a routine in the table, so the ranking
saw "no multi" and gave up: with Red the increment silently did nothing, and with a bare map
module it died with `Bogus postfix: ++`.

The ranking now looks at that bound dispatcher's candidates too. When none of them accepts the
operand (`Red::AST` against an `Int`), the core operator runs, as it does in rakudo, where the
imported multis join the core candidate set. With no routine of the name at all the core
operator also runs.

This was behind the `BBB.new(:value($++)) xx 10` expectation in Red's `t/40` and any Red test
that counts with `++`.

Refs #14094.
