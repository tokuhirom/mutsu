# A nested `whenever` over a synchronous supply emits its values

In a `supply` block whose `whenever` source is a cold, synchronous supply
(`Supply.from-list(...)`), a second `whenever` written inside the first one's body
passed its internal subscription marker to the tap as if it were a value, so
the tap printed `(Supply.new ...)` instead of `x1 x2 y1 y2`.

The outer body is replayed inside a frame that captures what it emits. That frame
is now marked as a capture frame. A `whenever` registered inside it taps its source
directly, so the inner values are captured in order with the outer body's own emits,
and `LAST` and `done` work as they do in Rakudo (#12985).
