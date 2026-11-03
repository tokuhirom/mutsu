# A dying downstream tap no longer quits the supply block that emitted to it

An exception thrown by a consumer's tap callback while a `supply` block's
`whenever` body was running `emit` was treated as the body's own error: the block
was quit and the consumer's `quit =>` handler fired, so every later event was
lost. rakudo hands the error back to whoever drove the emission and keeps the
block alive. The error from a downstream tap is now marked
(`RuntimeError::from_downstream_tap`) when an emitter's `emit` re-raises it, and
`call_supply_tap` no longer converts a marked error into `$emitter.quit`.

Found through `IO::Socket::Async::SSL` `t/client-server.rakutest`, whose server
tap body calls a method that does not exist on the connection: rakudo reports
the `isnt` of both connections (24 tests), mutsu stopped after the first (23).
The file now matches rakudo (24/24). Pinned by
`t/concurrency/supply/supply-downstream-tap-error-keeps-block-alive.t`.
