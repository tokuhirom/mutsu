# A synchronous `Supply.on-demand` quit arrives after the values emitted before it

`Supply.on-demand(-> $p { $p.emit(1); $p.quit("boom") })` delivered the quit
before `1` to a consumer that collects the body's values: a `whenever` in a
`react`, `await`, or a tap whose emits are not streamed. Rakudo delivers `1`
first (#11237).

Such a consumer only sees the body's values once the body returns, but
`$p.quit` ran the registered quit handlers at once, from inside the body. A quit
on the emitter of a body that is still running is now recorded on that body's
emit frame and returned as the body's failure, so it goes the same way as a
`die` out of the body: after the collected values. When the frame streams to a
tap (#11434) the values before the quit are already delivered, so the quit
still runs at once, inside the producer, as in Rakudo. Either way an `emit` or
a second `quit` after the first quit is dropped.
