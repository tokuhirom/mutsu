# A routine's `use` of a module that exports types no longer re-imports on every call

A `use` inside a routine body runs on every call. Since #13906, a later run of
such a `use` replays what its first run wrote instead of importing again, but
only for a module that exports plain routines. A module that also exports a
class, a subset or an enum paid the whole import on every call.

Imported type objects and enum values are the same on every run, so they now
replay too. When an enclosing import already bound the same name, the replay
records its shadow, as a real import does, and the scope's exit restores it.
The package type alias an imported type adds for the importing package is
memoized by table identity, like the routine and env alias tables. The scope's
exit now puts the very `before` table back when the importer's entry is the
only thing that changed, so the next call finds it.

`sub f($i) { use TM; $i + 1 }` called 20,000 times, where `TM` exports a
class, a subset and an enum (release build, same machine): 0.22-0.31 s
before, 0.058-0.074 s after, against rakudo's 0.18 s.
