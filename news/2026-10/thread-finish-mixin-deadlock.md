# `Thread.finish` no longer deadlocks against a role mixed into the thread

`$*THREAD does R` inside a thread first renames the Thread object to
`Thread+{R}` and then writes the role's attributes into it. `.finish`
snapshots the receiver's attributes instead of holding their read lock while it
joins, but it decided that from the class name `Thread`. When the joined
thread had already renamed the object, `.finish` held the read lock through the
join. The thread's attribute write then waited on that lock forever, and
`t/concurrency/thread-lock/thread-dynamic-var-identity.t` hung in about 4% of
runs under load.

The snapshot is now keyed on the method alone. Under the same load, 360 of 360
runs pass.
