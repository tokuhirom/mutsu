# A phaser that ends a routine is the routine's value

`sub t { INIT 5 }; say t()` printed `Nil` where rakudo prints `5`. The `INIT`/`CHECK` move of
ADR-0134 (#10552) deleted a statement-form phaser from the routine it took it from, even when that
phaser was the routine's last statement and so stood for its value.

A tail phaser now stores its value into a unit-level slot, as a value-form phaser always did, and
leaves a read of the slot behind. That covers a top-level `sub`, a method, and the method of a
top-level role.

The slot read is now decontainerized, the way the nested walk reads its own slots: `my @a = INIT
(1, 2, 3)` gave a one-element array (the list was assigned as one itemized value) and now has
three elements.
