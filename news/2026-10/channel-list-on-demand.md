# `Channel.list` and `.Seq` receive on demand

`Channel.list` and `Channel.Seq` used to drain the channel eagerly, blocking until it was
closed, so `for $c.list { ... }` ran its body only after the producer had closed the
channel. A consumer that has to react to an item before the producer sends the next one
(a request/response handshake over two channels) deadlocked.

Both methods now return a lazy list that performs one blocking `receive` per element,
reusing the on-demand pull machinery of `IO::CatHandle.lines` with a new
`ChannelReceive` mode. The loop body runs as each item arrives, the close ends the list,
and a `fail`ed channel rethrows its cause after the items sent before it. `.List` and
`.Array` stay eager. Three neighbouring wrong answers went with it: `$c.Seq` and `$c.List`
wrapped the Channel itself into a one-element list instead of draining it, and `$c.Array`
returned a List. Slipping (`|$c.list`) and `.Capture` force the on-demand list, as they
already did for a `gather`. (#12727)
