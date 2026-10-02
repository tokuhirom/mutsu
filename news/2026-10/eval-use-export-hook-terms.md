# EVAL accepts terms imported through a dynamic `sub EXPORT`

`EVAL 'use M <U>; U.k'` died with `Undeclared name: U` when `M` computes its
exports in a `sub EXPORT` hook from the `use` arguments (Logic::Ternary's
`t/04-export.rakutest`). EVAL's pre-run "Undeclared name" check runs before
the snippet's `use` statements load, and for a used module it only knows the
names a scan of the module's source finds; the names a `sub EXPORT` hook
returns exist only once the hook runs.

The check now asks whether a `use`d module declares a unit-scope `sub EXPORT`
(the same source test the parser's module scan uses) and, if one does, does
not judge the unit's barewords, the way the mainline undeclared-routine check
already steps aside for imports it cannot see. A real typo after such a `use`
is still rejected before the snippet runs, by the undeclared-routine check, as
in Rakudo. `need` runs no export hook, so it does not relax the check (#11062).
