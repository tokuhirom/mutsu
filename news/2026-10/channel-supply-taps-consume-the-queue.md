# `Channel.Supply` taps consume the channel queue

`$channel.Supply` lost every value sent before the first tap: `Channel.send`
emitted each value into a "bridged" live supplier at send time and also queued
it, so a value sent with nobody tapped was emitted to nobody, and a value a tap
did receive was still waiting in the queue for `poll`/`receive` to take a second
time. Against rakudo, eleven of twelve measured shapes diverged: a tap after the
sends (`[3]` instead of `[1 2 3]`), a tap after `close` or `fail`, `.map`/`.grep`
over a backlog, a `whenever` inside a `supply` block, and `.list`/`await` of a
closed channel's Supply, which hung (#9900).

`Channel.Supply` is now what rakudo defines it to be: an on-demand Supply. Each
tap runs a native producer that emits the backlog and then attaches the tap's
emitter to the channel as one more consumer of its queue; `send`, `close` and
`fail` only touch the queue and then pump queued values to the attached taps
round robin, completing them once the channel is closed and drained. There is one
delivery path, so a value leaves the queue for exactly one consumer -- a tap,
`receive`/`poll`, or a react `whenever` -- and every Supply combinator works on it
with no channel-specific code. The decision is recorded in
[ADR-9900](../../docs/adr/9900-channel-supply-taps-consume-the-queue.md).

On the way, an older bug surfaced: while a `supply { }` body ran, an `emit` on
any *other* `Supplier` was also collected as if the body had emitted it, so
`supply { $other.emit(5); emit 1 }` delivered `5` to its own tap. A body's emit
buffer now belongs to its own emitter only.

A further on-demand ordering bug -- a producer that calls `$p.quit` runs the quit
handler before the values it emitted -- is filed as #11237.
