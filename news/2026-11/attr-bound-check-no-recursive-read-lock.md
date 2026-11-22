# A private-attribute bind check no longer re-locks the instance

`nqp::eqaddr($x, $!attr)` and `$x =:= $!attr` ask whether `$!attr` was
`:=`-bound to a plain value. The check ran inside `with_self_attr`, which
already holds the instance's attribute read lock and hands over its map, but
it asked the instance again, taking a second read lock on the same `RwLock`.
When another thread's attribute atomic (`nqp::atomicinc_i($!n)`, `$!n⚛++`)
was queued for the write lock in between, the writer-preferring lock made the
second read wait behind the writer while the writer waited on the first: a
deadlock.

ParaSeq hit it on every degree > 1 `.map`. Its consumer tests
`nqp::eqaddr($next, $!lastsema)` while the batcher bumps `$!unhandled`. A
degree-3 `.map` over a phaser block hung in most runs, and `t/09-map` timed out
in the ecosystem sweep (#17723). The check now reads the map it was given.
