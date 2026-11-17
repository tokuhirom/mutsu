# A `use` reads what the loaded module provides instead of probing it

Since ADR-17558's first slice a `use` loads its module when the parser
reaches it, in the interpreter that runs the program. The parser now also
reads the loaded unit: routines whose names a module computes while it loads
(Moneys' `5USD` postfixes, bound into its export stash in a loop), the
precedence such an operator declared, and the declarators a module received
from a BEGIN-time `EXPORTHOW` merge (CompUnit::Util's `re-exporthow`). They
come from the module that was actually loaded, under exactly the tags the
`use` imports.

That retires the parse-time export probe of ADR-0124 and ADR-17557, which ran
such a module a second time in a fresh interpreter on a fresh thread just to
learn names. A probed module now runs once, so whatever it prints or writes
while loading happens once too.

The same change fixes the order rule slice 1 introduced: a module's static
export scan reported the scanned module's own `BEGIN` or `multi` as if the
importing unit had written it, which kept every later `use` of that unit from
loading at parse time.
