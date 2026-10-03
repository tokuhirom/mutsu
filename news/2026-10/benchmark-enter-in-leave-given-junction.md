# Benchmark's statistics: `ENTER` inside `LEAVE`, `Hash:D[T]` returns, junction `given`

Three fixes from the Benchmark distribution's `timethis(..., :statistics)`:

- **`ENTER` inside an exit phaser.** `LEAVE take now - ENTER now` evaluated the
  `ENTER now` when the `LEAVE` ran, so every measured duration was negative.
  An `ENTER` expression inside `LEAVE`/`KEEP`/`UNDO` is now hoisted to the
  enclosing block's entry, in routine and loop bodies alike.
- **`--> Hash:D[Duration:D]`** is canonicalized to `Hash[Duration:D]:D`, so a
  `my Duration:D %result` satisfies it instead of failing the return type
  check.
- **`given $junction -> Any $_ { ... }`** calls the block with the topic, so
  the Junction autothreads over the typed parameter instead of failing the
  bind (the same lowering `if COND -> sig { }` already used).

Both Benchmark test files now pass.
