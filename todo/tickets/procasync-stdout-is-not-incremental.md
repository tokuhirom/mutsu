# `Proc::Async` stdout/stderr arrive only when the child exits

A tap on `Proc::Async`'s `.stdout` receives nothing until the child process
terminates, and then receives the whole output as a single chunk. Rakudo streams
it as the child writes.

```raku
my $p = Proc::Async.new($*EXECUTABLE.absolute, '-e',
    q{$*OUT.print("EARLY\n"); $*OUT.flush; sleep 4; $*OUT.print("LATE\n");});
$p.stdout.tap(-> $c { say "[{now.Int}] OUT: $c" });
my $s = $p.start;
say "[{now.Int}] started";
await Promise.anyof($s, Promise.in(15));
```

mutsu prints `started` at T, then one `OUT: EARLY\nLATE` at T+4. raku prints
`OUT: EARLY` at T and `OUT: LATE` at T+4.

## Why it matters

Any handshake where the parent has to read something from a still-running child
deadlocks. The concrete case: a test that starts a helper server in a child
process, has the child report the ephemeral port it bound, and then connects to
it. `t/io-socket-async-real-connect.t` had to route the port through a file
instead, which is the workaround, not the fix.

It also means `.stdout.lines` can never fire early — and `.lines` on that Supply
does not fire at all, which is a separate bug
(`todo/tickets/supply-lines-drops-channel-backed-supplies.md`).

## Where to look

The Proc::Async implementation collects the child's output rather than pumping
it into the Supply as it is read; the reader needs to become a streaming reader
that emits each chunk, in the same shape as the real-TCP socket reader thread in
`src/runtime/native_methods/socket_async_conn.rs`
(`async_socket_supply_real_tcp`), which does emit incrementally.
