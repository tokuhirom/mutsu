# Derived Supplies over `Supply.interval` deliver their values

`Supply.interval(..).map(f)` / `.grep(p)` snapshotted the (empty) materialized
values of the channel-backed source and so never emitted anything, and
`.head(N).map(f)` / `.head(N).grep(p)` / `.head(N).Seq` did the same over a
`head` stage. A transform over a Supply that owns an event channel now taps it
per tap of its own, like one over an on-demand source (`is_channel_backed_source`),
a transform over a `head_limit` stage taps that stage, and `Supply.Seq` drains
through `supply_materialize` like `.list` (#17106).

The first tick can still be lost when a stacked stage's consumer attaches after
the source's buffered backlog is replayed; that startup race is tracked
separately.
