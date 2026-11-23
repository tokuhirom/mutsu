# A role punned by several threads at once keeps its methods

A role's pun class is composed the first time a method is called on the role
(`Monad::Maybe.some(1)`) and is found by name after that. The declaration
registry is shared by every thread, and composing a class takes several
writes to it: the class entry first, then its methods, accessors and composed
roles.

A second thread that asked for the same pun in between found the class by
name, took it as composed, and dispatched on it with half its methods missing.
Monad 0.3.0's `t/14_concurrent_types.t` lost worker threads to
`No such method 'some' for invocant of type 'Monad::Maybe'` this way. The
file's `ok` lines hid it, because a thread that dies leaves its error list
empty.

Composing a pun now takes one process-wide lock. The name being composed is
recorded until the class is complete, and a thread that finds it in that state
waits for the lock. The lock is the runtime of Raku's own `Lock`: it is
reentrant, and a thread waiting on it counts as stopped for the cycle
collector.

`t/concurrency/role-pun-concurrent-first-use.t` puns 60 roles for the first
time from 12 threads at once. It failed 7 of 8 runs before this change and
passes 20 of 20 now.
