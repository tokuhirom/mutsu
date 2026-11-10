# `start` no longer leaks a `%*x` / `@*x` binding past its scope

Spawning a thread published the spawner's aggregate dynamics into a process-wide
name lane, seeded only once. A `start` inside `{ my %*M = ...; }` therefore left
that hash visible to every later thread after the block was left. The spawn now
publishes no dynamic: the child's env clone already carries the live object.
Red's `t/028-events.rakutest` (event metadata) now passes.
