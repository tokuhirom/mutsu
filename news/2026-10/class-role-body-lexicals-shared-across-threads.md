# Class and role body lexicals are shared across threads

A class or role body's `my $n` was copied into each thread, so a method called on a
worker (`await start { $obj.inc }`) incremented the worker's copy and the main thread
never saw the change. A mixin type that `$o does R` created on another thread also ran
`R`'s body a second time when the main thread first used it.

Body scalars now live in a shared container. A thread that adopts another thread's mixin
type binds that thread's role-body lexicals instead of running the body again
(#12825, ADR-11456 §5).
