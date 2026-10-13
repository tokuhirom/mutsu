# The listener thread-count test no longer counts scheduler workers

`t/concurrency/io-socket-async-listeners-share-the-reactor.t` checks that more
`IO::Socket::Async` listeners add no threads. It counted every task in
`/proc/self/task`, so it failed about once in 30 release runs: the scheduler's
`pool` grows a worker lazily while the test awaits its listeners. The count now
skips `pool` workers. It still catches a per-listener socket thread, which is
what the test is for.
