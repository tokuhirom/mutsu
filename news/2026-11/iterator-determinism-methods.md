# Built-in iterators answer is-deterministic and is-monotonically-increasing

`(1..3).iterator.is-deterministic` died with `No such method` unless the program
composed `Iterator`, and then answered the role's defaults. A built-in iterator
now answers per kind as rakudo's does: not deterministic for a `.map`/`.grep`
stream, a hash's iterator and a `gather`; monotonically increasing for a
non-empty range. `(1..*).lazy.iterator` (monotonic) and `.pick(N).iterator`
(not deterministic) still differ and stay on #16941.
