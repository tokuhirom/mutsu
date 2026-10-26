# A block under a statement modifier in a term follows rakudo

In expression position (`({ 42 } if 1)`) a bare block under `if` / `unless` / `given` / `with` /
`without` / `when` is now the uncalled Block value, as in rakudo 2026.09; a pointy block under `if`,
`unless`, `with` or `without` is called with the tested value (`(-> $x { $x } with 42)` is `42`), and
under `given` it stays a closure. Under `while` / `until` a bare block now runs on every pass and
its value is collected (`({ 1 } while $i++ < 2)` is `(1, 1).Seq`); at statement level
`{ $n += 2 } while ...` used to run nothing and now runs.

The statement-level form is unchanged (`{ $n = 5 } if 1;` runs the block). `while_operand`, which
turned the block into an uncalled closure for `while`, is deleted, and the `RakuAST` round trip
(`.AST` / `EVAL`, `MUTSU_RAKUAST=convert`) agrees through `lower::lower_modified`, which tells the
term form from the statement form. `t/control/bare-block-for-statement-modifier.t` encoded the old
answers and now matches `raku`. Fixes #14941.
