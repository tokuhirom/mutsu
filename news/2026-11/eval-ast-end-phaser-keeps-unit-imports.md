# EVAL($ast): an END phaser keeps its unit's `use` imports

An `END { ... }` an `EVAL`'d unit installs runs at program exit, when the unit's import
scope is closed, so a call to a routine its own top-level `use` imported died with
`Unknown function`. The phaser now carries the lexical import sets open where it was
installed and the exit-time run reads them (`EndPhaser::import_chain`), as it already did
for the routines the unit declared. `imported-toplevel-use-unit-set.t` joins the
`EVAL($ast)` ratchet.

Not covered: the same END in a unit `EVAL`'d from a *string* (`EVAL $str`): its `use`
runs in the pre-run's import region, which the phaser does not capture (#13441 area).
