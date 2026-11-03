# An EVAL'd unit's type arguments see the types its `use` statements import

`EVAL 'use NativeCall; has Pointer[Pointer] $.p'` was refused with `Undeclared name:
Pointer`: the check for undeclared type arguments knew only the types the unit declares,
while the undeclared-name check beside it also counts what the unit's own `use`
statements import (they have not run yet when the checks do). Both now ask the same
question (`eval_declared_types`), and a unit that imports through a `sub EXPORT` hook is
not judged, as in the name check.
