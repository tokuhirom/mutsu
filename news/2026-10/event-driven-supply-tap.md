# An idle Supply tap no longer holds a thread

Tapping a channel-backed live Supply — an `IO::Socket::Async` connection's
`.Supply`, a listener, `Supply.interval`, `signal`, a `Proc::Async` stream, or a
tap scheduled on the `ThreadPoolScheduler` — used to submit a consumer to the
worker pool that blocked on the channel for the tap's whole lifetime and
re-checked its close flag every few milliseconds. A process with N tapped
connections therefore ran about N pool threads, all waking hundreds of times a
second: ten idle connections showed `pool(21)`, and the LLM::Chat test suite's
~1000 keep-alive connections were OOM-killed at ~13 GB (#12892).

The act loop is now event-driven. A callback-style hook on the channel submits
one drain task when an event arrives and none is scheduled; the drain dispatches
whatever is queued and returns its worker. A pair of flags keeps the drains of
one tap serial, so the tap's callback still runs one body at a time and in
order, and no wake-up is lost between the last read and going idle. `Tap.close`
sets the flag and kicks the loop, which retires at once instead of within 250 ms.

The same ten idle connections now show `pool(3)`, and the pool threads wake
zero times a second instead of ~4000.
