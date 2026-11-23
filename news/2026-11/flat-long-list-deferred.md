# `.flat` of a long List is O(1) to create again

Since `flat` learned to read an `Iterable` instance through its own
`iterator` (#15322), `.flat` on a `List` scanned every element up front for
such an instance, and the method dispatch walked every element again for a
nested `.map`/`.grep` Seq. `$list.flat.head(1)` was therefore O(n) where
rakudo's lazy `flat` is O(1) (#15962).

A `List` longer than 64 elements, or a short one that holds an instance, now
flattens through a deferred pull. The deferred-`.map` source gains a
callback-free `Flat` mode that flattens each element only when a pull reaches
it, and it reads an instance's `iterator` and reifies a nested Seq at that
point. Creating the `.flat` does no per-element work, and an instance's
`iterator` now runs on consumption rather than at the `.flat` call, as in
rakudo. A short plain List keeps the pure cursor. A debug build measures
`(1..n).list.eager.flat.head(1)` at about 0.1 ms for n from 1,000 to
3,000,000. Before the change, n = 1,000,000 took 21 ms.
