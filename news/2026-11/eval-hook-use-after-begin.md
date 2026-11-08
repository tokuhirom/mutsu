# EVAL pre-runs a `sub EXPORT` hook module's `use` past a BEGIN block

`EVAL 'BEGIN { 1 }; use M <U>; nosuch'` reached the unit unjudged: the pre-run of the unit's
top-level `use` statements (#13441) stopped at the first BEGIN, so a hook module's names were
unknown and the undeclared-name check gave up. Only a hook module (whose names depend on running
it) now runs ahead past a BEGIN; a plain module keeps its load in place.
