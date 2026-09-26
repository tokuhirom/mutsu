# A tap callback no longer sees its own `my $tap` unbound

`my $tap = Supply.interval(0.05).tap({ ...; $tap.close })` could die with
"No such method 'close' for invocant of type 'Any'" under load
([#9590](https://github.com/tokuhirom/mutsu/issues/9590)). It was seen as a CI failure of
`closure-self-capture-call-arg-cross-thread.t`: 3 of 24 runs failed when eight copies ran in
parallel.

The callback is created inside `$tap`'s own initializer. Since #9493 it reads `$tap` through a
shared cell, but the cell holds the Tap only after `.tap` returns and the declaration stores it.
A channel-backed tap (an interval, a signal, a socket) runs its consumer on a pool worker, and
the interval's timer was already running. When the first tick won the race, the callback read
`Any`. Rakudo has the same window in principle. It rarely loses there because its `tap` returns
long before the first cued tick runs.

The ordering is now structural instead of a matter of timing. When the compiler finds a
self-captured declaration, it also marks the closures its initializer creates
(`CompiledCode::captures_own_declaration`). A tap whose callback is such a closure does not
submit its consumer. It holds the consumer in a per-thread list (`runtime::decl_gate`), and the
declaration's store submits it. This covers the plain channel-backed tap, a `supply` block's
`whenever` reader, and the scheduler-pump drain.

A held consumer is also released at the thread's next blocking point
(`worker_pool::enter_blocking`, which every `sleep`, `await` and join goes through).
An initializer that waits for its own callback would otherwise deadlock. In that case the
callback sees the declaration unbound, which is what Rakudo does.

Pinned by `t/concurrency/thread-lock/cross-thread-tap-callback-waits-for-its-declaration.t`. It keeps
the initializer busy after `.tap` returns, so without the hold it fails every time instead of
only now and then.
