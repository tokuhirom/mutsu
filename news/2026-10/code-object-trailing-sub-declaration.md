# A routine called through its code object returns its trailing `sub` declaration

`my sub pw { sub tw($x) { $x } }` returns the `tw` Sub when called by name, but
`&pw()` returned `Nil` (or failed a `--> Code:D` check): a named routine called
through its code object runs on the closure dispatch path, which never built
the trailing-declaration value the named call path does. Both paths now share
one helper (`trailing_sub_decl_value`), so the two calls agree, and a bare or
pointy block ending in a `sub` declaration returns the Sub too.

The returned Sub also no longer inherits the declaring routine's return type
from the flattened env: `sub f(--> Code:D) { sub g($x) { $x } }` called through
`&f` checked `g`'s result against `Code:D`. It now carries its own `--> T`
only, the rule closure creation and `EVAL` already follow (#13040).
