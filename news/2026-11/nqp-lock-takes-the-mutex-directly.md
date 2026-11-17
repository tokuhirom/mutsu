# `nqp::lock` / `nqp::unlock` take the mutex directly

`nqp::lock($lock)` and `nqp::unlock($lock)` used to dispatch `.lock` /
`.unlock` by name through the interpreter's `call_method_with_values` cascade.
That walk ends in the same `lock_lock` / `lock_unlock` that the `Lock` method
rows call. A `Lock`-family object (one carrying its `lock-id`) is now locked
directly, the way MoarVM's op takes the mutex without a method call. Anything
else keeps the by-name dispatch.

AttrX::Mooish takes this path in `attr-var`, around every lazy attribute's
proxy installation. A lock/unlock pair dropped from 21.8k to 8.2k
instructions (callgrind slope, profiling build).
