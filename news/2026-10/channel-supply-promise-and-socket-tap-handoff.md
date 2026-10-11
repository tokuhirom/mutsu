# Channel sources in `supply {}.Promise`, and socket reads that wait for a tap

Two gaps that kept Log::Timeline's socket output suite red (#12750):

- `supply { whenever $channel.Supply { ... } }.Promise` treated a channel-backed
  source as an already-finished on-demand body, so the promise was kept before any
  later `send`. The `.Promise` path now drains a `Channel.Supply` source from its
  drive loop, the way the `react` loop does.
- Each `$conn.Supply` on an `IO::Socket::Async` connection started a reader thread
  that kept consuming the socket after its `react` had finished, so the next tap
  lost chunks nondeterministically. The reader now peeks, and retires once every
  tap is gone, leaving the data for the next tap as rakudo does.

All five Log::Timeline test files now pass under mutsu, next to `raku`.
