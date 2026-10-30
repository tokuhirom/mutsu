# A `whenever` handler resolves dynamic variables through the emitter

On rakudo a live `Supplier.emit` runs each tap's callback inside the emit call,
so `$*dyn` in a `whenever` body sees the emitter's bindings. mutsu runs the
handler on the react's own interpreter, so it saw the react creator's instead.
The emitter now publishes a snapshot of its dynamic variables with the event
(only when a `react` is listening) and the drive loop overlays it for the
duration of the handler. Fixes Red 0.2.7 `t/28-events.rakutest` (#15497).
