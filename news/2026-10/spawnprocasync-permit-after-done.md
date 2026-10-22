# `nqp::permit` after `done` still reads a child's buffered output

`nqp::spawnprocasync` dropped the child's task from its registry as soon as it
pushed `done`, so an `nqp::permit($handle, 1, -1)` given afterwards found no
task and did nothing: the output the child had already written to the pipe was
never delivered, and the stream never ended. MoarVM keeps the pipe as long as
its handle exists, and Rakudo delivers the buffered chunk and the end of the
stream. The registry now keeps a reaped child's task while one of its output
streams can still be read (its pipe was never taken, or its reader waits for
another permit) and drops it when the last such stream reaches its end. Tasks
kept for streams nobody ever permits are bounded (the oldest 256 stay), so they
cannot grow the registry without limit. This is the empty payload seen in
`t/concurrency/nqp-spawnprocasync.t` under a loaded `prove -j` run (#13513).
