# A role method's escaping closure sees the role's `&` parameters

A closure a role method returned (a `Proxy` FETCH, a callback, `sub () { ... }`)
ran after the method's frame was gone, and its by-name `&rel1(...)` call found
nothing: the compiler hands a closure the enclosing `&name` bindings to capture,
but a role's own parameters were not among them, so `role R[&rel1] { method m {
-> { rel1(7) } } }` died with "Unknown function: rel1" (`&rel1` as a value read
back `Nil`). Role `&` parameters are now captured like any outer `&name`.

Clears the `Unknown function: rel1` cluster of the Red 0.2.7 distribution
(`Red::Attr::Relationship`'s relationship Proxy, #14094).
