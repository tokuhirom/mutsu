# `Supply.throttle` works over a live source

`throttle` used to read the source's materialized values, so over a live
(Supplier-backed) source it emitted nothing. It now has a live stage, built
once per tap like the other live combinators (ADR-18443):

- `throttle($limit, $seconds)` passes at most `$limit` values per window and
  queues the rest for the windows after it. `done` waits until the queue is
  drained.
- `throttle($limit, &block)` runs the block on each value with at most
  `$limit` jobs at once, and emits one Promise per job, kept with the block's
  result.

With this, every Supply combinator over a live source is a per-tap recipe.
Only the consumers (`first`, `.Promise`, `.wait`) instantiate the chain once
per call (#18579).
