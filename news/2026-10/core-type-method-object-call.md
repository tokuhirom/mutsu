# Calling a core type's Method object, and positional `Map` subclass construction

`Map.^lookup('new')(Map, ...)` used to die with "Unknown function: new", because a
core type has no registry class and the call looked up a *sub* of that name. A native
Method object of a core type now dispatches on its invocant: a type-object `new` runs
the core constructor (never a subclass's own `new`, so `method new(|c) { &new(self, |c) }`
no longer recurses), and an instance invocant runs the owner-qualified native method
(`Map::keys`, `Map::raku`). `self.Map::new(|c)` from a subclass `new` reaches the core
constructor instead of recursing until the stack overflows, and `class C is Map {}`
accepts a positional list in `.new`.

Found with the `immutable` distribution (via `ValueMap`). `ValueMap.new` now builds; the
distribution's `t/01-basic.rakutest` still needs a user `is Pair` subclass to have
`key`/`value` and a Map subclass's `.raku` to name the subclass.
