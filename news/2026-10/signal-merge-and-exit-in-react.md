# `signal().merge()` stays live, and `exit` in a signal `whenever` ends the process

`signal(SIGTERM).merge(signal SIGINT)` used to complete immediately: `Supply.merge`
snapshotted the (empty) `values` of channel-backed sources. A merge with a
channel-backed source (`signal()`, timers, process streams) now pumps the sources'
events into a channel-backed merged supply, `done` when every source is and
`quit` with the first that quits.

Separately, `exit` inside a `whenever` body only latched the halt flag; the
`react` drive loop kept parking on its waker. It now stops once the interpreter
is halted, so the process ends (#13472).
