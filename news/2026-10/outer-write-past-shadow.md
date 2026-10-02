# `$OUTER::x` reaches the outer binding past a shadowing `my $x`

A write through `OUTER::` now reaches the binding `OUTER::` names even when a
scope in between declares its own `$x`. In Rakudo
`my $x = 1; sub s { my $x = 3; my $y; $OUTER::x := $y; $y = 5 }; s(); say $x`
prints `5`, and `my $x = 1; { my $x = 2; $OUTER::x = 7 }; say $x` prints `7`;
mutsu printed `1` for both, because the write fell through to a literal
`OUTER::x` key. A read past a routine's own `my $x` (`sub s { my $x = 2;
$OUTER::x }`) returned the routine's binding instead of the outer one; it is
fixed too (#10827).

The plain name cannot reach a shadowed binding -- inside the shadow it is the
shadow's, in its slot and in its env entry alike -- so the target gets a key of
its own. Its slot is given a shared cell (a binding cell when a `:=` reaches
it, ADR-0097 §14), published in the env under `__mutsu_outer::<scope>:<name>`
by the new `BoxOuterRef` op, and the write is an ordinary by-name store of that
key. A target in the same frame is published at the write site; one in an
enclosing frame is published by the frame that declares it, right before it
creates the closure or sub, and the closure capture carries the key. A `my $x`
the unit writes through `OUTER::` anywhere is shared in a cell at its
declaration, while the name still denotes it, so the slot, the env entry and
the block/loop exits that re-seed an enclosing slot from the env all see the
same cell.

Along the way `++`/`--` through a binding cell stepped the cell instead of the
container it holds, so `my $a = 1; my &c = { $a++ }; $a := $b; c()` lost the
increment; it now steps the container.

Two neighbouring gaps are tracked separately: a whole-container store to a
shadowed `@OUTER::a` / `%OUTER::h` (#10857), and the block exit that re-seeds
every same-named enclosing slot from one env value (#10856).
