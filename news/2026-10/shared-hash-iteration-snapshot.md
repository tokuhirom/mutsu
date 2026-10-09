# Iteration and callback methods on a shared Hash/Array run on a snapshot

`.sort`, `.map`, `.grep`, `.first`, `.gist`, `.Str`, `.raku`, `.join` and a few
other methods that run user code over the elements of a `Hash`/`Array` crashed
(SIGSEGV, OOM abort) when another thread inserted at the same time. They cannot
hold the container stripe, because a callback may block on another thread
(ADR-0068 §7.4). Once a second mutator thread exists, they now take a shallow
snapshot of the container under the stripe, release it, and run on the snapshot
(`container_lock::shared_snapshot`, ADR-0068 §16). Programs that never spawn a
thread are unchanged. Fixes #12456.
