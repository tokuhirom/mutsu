# `FIRST` fires in a bare block or an `if` body

`for ^3 { if True { FIRST say "if" } }` now prints `if` three times, as
on rakudo. A block inlined into its enclosing code is a fresh clone each
time it is entered, so its `FIRST` runs on every entry. Such blocks are a
bare block, an `if`/`else`, `when`/`default` or `try` body.

Before, only an inlined block that also had an `ENTER`/`LEAVE`-family
phaser compiled as a phaser block scope. A block with only a `FIRST`
took the plain inline path, where the `FIRST` compiled to nothing.

A `FIRST` now also routes such a block to the phaser block scope,
which runs it at entry (#17992). A callable's own body and a loop body
keep their per-clone and per-loop handling.
