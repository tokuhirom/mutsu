# Thread-shared array mutators answer through `Array`'s rows

The VM's method-call opcode kept its own copies of `push`, `unshift`, `append`,
`prepend`, `pop`, `shift` and `splice` for an `@array` that several threads
reach. That lane is now the first step of `Array`'s method-table rows
(ADR-11276 §9.169), so every entry that mutates a named array, a run-time
method name included, writes to the array's one shared home.
