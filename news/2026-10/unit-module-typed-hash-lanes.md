# A `unit module`'s typed hash keeps its own key type across modules

Two modules that each declared a file-scope `my %seen{...}` with a different
key type shared one key-type constraint: the declared types live in env
metadata lanes, the module body runs in its loader's env, and nothing took
those lanes out when the load ended. The last module loaded won, and the first
module's subs died storing a key of their own declared type
(`expected Hash but got List`).

Leaving a unit body now records the element and key types of its own `@`/`%`
lexicals per unit and puts the importer's lanes back, the way the values move
into the unit's lexical store. A store from the unit's routines checks the
running unit's record ahead of the callers' frames, and a routine's own
same-named declaration still comes first (#13086).
