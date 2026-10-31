# A negative literal subscript is reported when the compile phase ends

`@a[-1]` and `@a[0..-1]` are a "sorry" of rakudo's grammar: the parse goes on,
the unit's `CHECK` code still runs, and the compile phase then fails with
`===SORRY!=== Error while compiling FILE` -- next to whatever `CHECK` code
queued with `$*CU.add-sorry`. mutsu stopped at the parse, so `static`'s own
report (`[static:mismatch] ... index outside dimension 1`) never appeared.

The parser now queues the diagnostic (`parser::soft_sorry`) and keeps parsing;
when the unit has a `CHECK` phaser it is handed to the unit's compile-phase
queue with its position (`unit_diagnostics::add_parse_sorry`), and a program
without one fails at once with the same error as before.

`static` 0.0.1 `t/15-containers-and-effects.rakutest` goes from 2 to 0 `not ok`,
so every file of its suite matches rakudo (#15610).
