# A new `react` over a `Supplier` no longer replays what a finished `react` consumed

`react { whenever $supply { ...; done } }` run twice over the same `Supplier`'s Supply handed
the second one the values the first had already taken: the push-sink registration replays the
supplier's whole emit buffer so a producer that races ahead of a react body's `whenever` loses
nothing, and nothing ever advanced past what an earlier sink had received.

The supplier now remembers where the buffer stood when its last sink was unregistered and a
later sink replays from there. The race-covering replay (values emitted before the first sink
exists) is untouched, as is `Supplier::Preserving`'s own watermark; 424 files under
`t/concurrency` pass unchanged.

This was Red's `Red::Class.instance.events` (one `Supplier`, a singleton): the second `start
react whenever Red.events` of `t/28-events` received the first test's `42`, so its
`isa-ok $event.data, Red::AST::CreateTable` saw an `Int`. What a plain Supplier emits while
nobody is listening still reaches the first react that taps it, unlike rakudo -- filed.

Refs #14094.
