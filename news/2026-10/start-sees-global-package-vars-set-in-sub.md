# A started thread sees package variables a sub assigned

`$GLOBAL::x = ...`, `%GLOBAL::h = ...` and `@GLOBAL::a = ...` made inside a sub live in the
persistent package-variable store, not in any frame env. A thread forked by `start` began with an
empty store, so it read `Nil`/empty for them (and for the `*` dynamic spellings). The fork now
carries the parent's store as of the spawn; containers keep sharing their cell with the parent.
This unblocks Red t/73 (`$*RED-DB` set inside a sub, read inside `start { react { ... } }`).
