# An `.^add_method` closure installs only the names its body reaches

A method installed from a closure literal (`$class.^add_method('LOCK', my method () { ... })`)
keeps the whole scope it was written in, and every call inserted each entry of that
scope into the frame env, then merged the overlay back on return. A call now installs
only the names the method can look up: its body's free variables, locals and the names
its ops carry (memoized per compiled chunk). A body that creates closures or names
lexicals reflectively, and a signature that can read the scope (a type capture, a user
type, a default or a `where` clause), keep the whole scope.

Method::Protected's generated `LOCK` method went from 40.9k to 29.4k instructions per
call; AttrX::Mooish's generated clearer and predicate methods take the same path.
