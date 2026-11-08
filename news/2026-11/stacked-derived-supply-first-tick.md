# A stacked derived Supply no longer loses its first tick

A derived Supply stacked over `Supply.interval` (`.head(3).map(..)`, `.map(..).head(2)`)
registered its consumer on the producer's emitter only after the derive shim returned. A tick
emitted by the interval's thread in that window was recorded in the emitter history and delivered
to nobody, so the consumer saw `(11, 12)` instead of `(10, 11, 12)` under load. Registering the tap
and reading the history that was not already streamed to the consumer is now one step against every
emit (`emit_gate`), and the missed values are delivered to the new tap first. (#17167)
