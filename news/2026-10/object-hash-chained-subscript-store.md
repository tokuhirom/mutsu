# A chained store through an object-hash key reuses the key's entry

`%h{$obj}[3] = v` (and `//=`, `||=`, `++`, `+=`, `%h{$obj}<k> = v`) on an object
hash (`my %h{M}`) looked up the first-level entry by the key's string form
instead of its `.WHICH`, so every chained store filed a second, string-keyed
entry and later reads of `%h{$obj}` never saw it. CSS::Properties' per-module
property cache (`%module-properties{$module}[$n] //= ...`) therefore never hit,
and every `$css.info(...)` rebuilt a `PropertyInfo` (about 8 ms each).

Also: `collect_handle_ids`, which runs when a worker thread starts, no longer
holds an object's attribute read lock while it follows the object graph. A
cycle back to the same instance took a second read lock on the same `RwLock`,
which deadlocks once another thread queues a write in between (seen as a hang
in CSS::Properties `t/threads.t`).
