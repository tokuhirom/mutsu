# A `.grep(...).map(...)` chain interleaves its callbacks per element

`@l.grep({ m/ '#|' \s* (.*) $/ }).map({ ~$0 })` answered `["y", "y"]` where
Rakudo answers `["x", "y"]`: the map stage reified the whole grep before its
first callback ran, so every map callback read the `$/` of the *last* match
(#11176, found through CSS::Writer's `t/node-doco.t`).

A `.map` or `.grep` called on a `.map`/`.grep` Seq whose callback has not run
yet now chains onto it instead of reifying it. The receiver's deferred source
is stolen into the new stage as `MapGrepItems::Chain`, and pulling the new
stage pulls the upstream one element at a time, running the downstream
callback over each element as it arrives — Rakudo's pull pipeline. The
observable order of side effects matches Rakudo too (`g1 g2 m2 g3 g4 m4` for
`(1..4).grep(...).map(...)`), a `last` in the downstream callback stops the
upstream one, and a `for` loop over the chain interleaves all three
(`G1 M1 B1 G2 M2 B2 ...`). Chaining consumes the receiver, as any `.map` on a
Seq does.

The per-element pull costs more than the old bulk pass: a 200 000-element
`@a.grep(...).map(...)` takes 0.43 s against 0.19 s for the same work split
into two statements (Rakudo: 0.32 s). That overhead is the per-chunk setup
every single-element map/grep pull pays, `for @a.grep(...)` included, and is
tracked separately.
