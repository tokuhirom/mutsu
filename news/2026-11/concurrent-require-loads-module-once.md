# A module loads once when several threads `require` it at once

The table of loaded modules has been shared by every thread since ADR-17216
slice 3, so a thread that `require`s a module another thread already loaded
finds it loaded. But checking the table and loading the module were two
separate steps. Four `start` blocks that each ran `require M` at the same
moment all checked before the first load finished, so all four ran `M`'s
body, including its `BEGIN` and `INIT` blocks.

Each load entry point (`use`, `need`, `require`, `CompUnit::Repository.need`
and the compile-time `use`) now first takes a *claim* on the module's name. A
second thread waits for the first thread's load to finish and then takes the
already-loaded path. The claim is per module, not one global lock, so a
module whose mainline awaits a `start` that loads another module still
finishes, as on rakudo. A thread whose wait would close a cycle of such waits
does not wait, so two threads that `require` each other's module get the
circular-dependency error instead of rakudo's hang (rakudo-divergences A5).
