# Loop kernels call sub kernels directly

A loop kernel (ADR-19288 stage 2) now admits a call to a sub that runs as a sub
kernel (stage 1): the call is a direct machine call with machine-integer
arguments, and a bail in the callee (an overflow, a zero divisor, the depth
bound) bails the loop iteration, which the driver reruns generically. Each call
is resolved when the loop kernel is entered, through the same name-keyed
light-call cache the generic call takes, after every probe that would claim the
call first (a frame-lexical routine, a frame-activation binding, a dispatcher
builtin, an `nqp::` op, a qualified name, an empty-signature proto, the cache
bypass list); a call that resolves anywhere else keeps the loop generic.

Because a sub's first call fills that cache, a counted `for` that declines at
its first iteration is tried again after 256 iterations, as a `while` already
was.

`benchmarks/debug-guard.raku` (`$sum += step($i) mod 1000` over 100,000
iterations) went from 961.9M to 36.8M instructions with kernels on, and from
69.7 ms to 10.0 ms wall, against rakupp v5.9.0's 51.1 ms (minimum of 7 runs on
the 4-core container). Before this change its loop entered the `step` kernel
from the generic call path 99,900 times.
