# `Supply.interval(1).list` returns its ticks as they come

`.list` and `.Seq` of a supply backed directly by a native event channel --
`Supply.interval`, an `IO::Socket::Async` read stream, the other `supply_id`
sources -- drained that channel until it closed, so `for
Supply.interval(1).list { last if ... }` never returned. They now subscribe to
the channel and answer a lazy list that waits for one event per element
(`CatPullMode::SupplyReceive`); the stream's done ends the list and a quit is
thrown where it occurred. The read stream of an already closed socket still
lists nothing at once.

Making these lists lazy exposed that `append` and `prepend` did not read an
on-demand pull list: `@a.append: $channel.list` added one empty List (`[()]`),
on a plain array and on an outer array appended to from another thread. Both
paths now read such an argument to its end before flattening it, as rakudo
does.
