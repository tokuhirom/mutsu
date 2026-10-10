# Variable.block and Block.add_phaser for variable trait handlers

A `trait_mod:<is>(Variable:D ...)` handler can now call `$v.block`, which answers a
`Block`, and `$v.block.add_phaser("ENTER", { ... })`. This is what the Injector
distribution's `is injected` variable trait does to seed the variable on block entry.

The handler of a nested declaration already runs once, in the BEGIN prologue. The phaser
it adds is filed under the declaration, and the declaration replays it on every entry of
its block, with `$v.var` bound to that entry's variable. A trait applied at the
declaration itself (unit level, package bodies) runs the phaser at once.

The phaser runs where the declaration is, after the statements that precede it, where
rakudo runs it before the block's body. Only `ENTER` is supported; ADR-12131 records the
design that would remove both limits.

`Variable.var.defined` now reads through to the variable's value, so `without $v.var`
works in a handler, and a value assigned through `$v.var` is visible to a later read of
`$v.var` in the same handler.
