# A class declared in a routine is one type, even under threads

A package-scoped `class Res { ... }` declared inside a routine was registered
afresh on every call. Rakudo creates such a type once, at compile time, and
mutsu already kept a `my` class and an anonymous `class :: { ... }` per
declaration site, but not a plain (`our`) one. Rebuilding it cleared and
refilled its method table, which other threads were dispatching through, so a
concurrent caller found no accessor: Proc::Q sends
`class Res { has $.out ... }.new: ...` from each `start`ed process handler,
and t/03-in.t intermittently died with "No such method 'out' for invocant of
type 'Proc::Q::Res'". Every static declaration site now keeps its type, so
re-entry only runs the body's per-entry statements; Proc::Q's suite matches
rakudo.
