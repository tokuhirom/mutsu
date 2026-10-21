# Slipping an infinite gather no longer hangs

`slip(gather { loop { take ... } }).head(3)` and `.Slip` on an infinite gather
used to hang. `slip(...)` read its gather argument whole, and `.Slip` copied
the gather's elements. A gather's Slip now gets the bounded pull a gather
returned from a `.map` callback already gets:

- A gather that finishes within the pull slips its elements eagerly, as
  before.
- A gather that keeps running becomes a lazy-tail Slip. That Slip carries a
  lazy pipe over the gather, so `.head(3)` and `(1, slip(inf)).head(3)` read
  only a prefix.

A lazy-tail Slip, such as the Slip of a `.lazy` list, now also flattens where
Rakudo flattens it:

- into a `.map` callback's result;
- into `take`;
- into a `flat` argument.

Before, `(1, 2).map({ slip((gather { take $_; take $_ }).lazy) })` rendered
`((...) (...))`. It now gives `(1 1 2 2)`. One chokepoint,
`Interpreter::reify_lazy_slip_tail`, does that flattening (#13117).
