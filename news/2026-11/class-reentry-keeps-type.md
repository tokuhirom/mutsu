# A class declared in a routine keeps its type when its body has statements

A `my class` (or an anonymous `class :: { ... }`) declared inside a routine is
one type per declaration in Raku: `sub f($i) { my class C { my $z = $i } }`
gives `f(1) === f(2)`, and only the body's statements run on each call. mutsu
already kept the type for a purely declarative body (#16978), but a body with
any statement -- a `my` variable, a nested `sub`, a LEAVE phaser -- was rebuilt
from scratch on every entry: method rows cleared and re-inserted, roles
recomposed, every method cache dropped.

Since the type registry is shared by every thread (ADR-17216), that rebuild was
visible half-way to other threads: eight workers each calling a routine that
declares `class :: { my $z = 1; method put(|) {...}; method print(|) {...} }`
saw `.put` and `.print` fall through to `Mu`'s methods while another worker
was refilling the method table (#18070).

Such a declaration now registers once and is stamped like a static one; a
later entry runs only the body's per-entry statements in the class scope
(`run_class_body`'s per-entry mode), then refreshes the methods' captures of
the current frame. A body statement that can change the class itself (`use`,
a nested type, BEGIN/CHECK, an `EVAL`) keeps the declaration on the full
rebuild path.
