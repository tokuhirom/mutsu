# `Supply.list`, `.Seq` and `.Channel` deliver values as they are emitted

`Supply.list` and `.Seq` of an on-demand `supply { }` block or of a supply
derived from a live one used to tap the
supply and collect every value until it was done, giving up after a 30-second
deadline. A `for $supply.list { last if ... }` over a supply that never
finishes therefore stalled for 30 seconds: EventSource::Server's `out-supply`
merges its events with a `whenever Supply.interval(60)` keep-alive, and its
`t/020-basic.t` took 30.1 s against rakudo's 0.6 s.

Both methods now follow Rakudo's definition: they tap the supply with collector
shims that feed a channel and answer that channel's lazy list, so each element
is available as soon as it is emitted, `done` ends the list, and a `quit` is
thrown after the values emitted before it. `Supply.Channel` is built the same
way (Rakudo's `self.tap(-> \v { $c.send(v) }, done => { $c.close }, quit =>
{ $c.fail($_) })`), which also fixes a dying supply block failing its channel
ahead of the values it had already emitted. A supply backed directly by a
native event channel (`Supply.interval` itself, a socket read stream) still
drains that channel eagerly; its lazy form needs the channel to back the list.

EventSource::Server's five test files now match rakudo, `t/020-basic.t` in
0.1 s.
