# The thread `nqp::` ops

The seven ops of NQP's Threads family are implemented: `currentthread`,
`newthread`, `threadrun`, `threadjoin`, `threadid`, `threadyield` and
`threadlockcount` (#11502).

In Rakudo these ops are the VM layer under `Thread`. mutsu's `Thread` is
native and acts as its own VM handle, so each op runs the matching `Thread`
operation on that same object:

- `newthread` is `Thread.new`;
- `threadrun` is `.run`, on the same spawn path with its `clone_for_thread`;
- `threadjoin` is `.finish`;
- `currentthread` answers the same object `$*THREAD` does.

There is still only one way to start a thread.

`threadlockcount` needed a count of the locks each thread holds, and mutsu's
`Lock` did not keep one: it only recorded the owning OS thread. A per-thread
counter now changes wherever lock ownership changes: the first acquisition,
the final release, and the release and re-acquire around `Condition.wait`.
It counts a re-entered lock once, as MoarVM does. Any thread can read
another thread's count.

Writing the test showed that the first `Thread` a program creates got id 1,
the same id as the initial thread. Thread ids now start at 2.
