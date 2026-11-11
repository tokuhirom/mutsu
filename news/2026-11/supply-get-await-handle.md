# `Supply.get-await-handle`

`Supply.get-await-handle` now returns a real `Awaitable::Handle`, completing
ADR-15519's adapters (`Promise` and `Channel` got theirs in #17195). The handle
is never settled up front; its `subscribe-awaiter` taps the supply and settles
with the last value emitted before `done` (`Nil` for none), or with the
exception the supply quits with -- exactly what `await $supply` yields. Before,
the method did not exist on a `Supply`, so code that drives the protocol by
hand (a custom `Awaitable` delegating to a supply) died with "No such method".
