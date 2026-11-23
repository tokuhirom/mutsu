# A slang's productions run in the program's own interpreter

A slang module the program loads at parse time now has its tokens and actions
run by the program's own interpreter, as nested calls during the parse
(ADR-17558 slice 4b, §7.18). Before, they ran in a separate slang host
interpreter on its own thread.

An action's side effects now happen once, when the parser reaches the
production, as on rakudo. A `BEGIN` later in the unit sees them; before, it
saw nothing until the unit replayed the action at run time. A module that
`use`s a slang for its own compile no longer passes that slang's productions
on to its importer through the shared registration lists.

Slangs that override a declarator (`routine-declarator`, `package-declarator`)
keep the separate host for now. Their actions work on the code object the
unit builds later.
