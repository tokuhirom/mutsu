# `for` over `Seq.new($iterator)` pulls one element per iteration

A `for` loop over a `Seq` built from a user `Iterator` used to drain the whole
iterator before the first body ran, so iterator side effects (LN's `.lines`
counter) were all visible at once. The loop now claims the Seq's iterator source
the way it already claimed a `.map`/`.grep` source and pulls one iteration's
elements at a time.

Together with the `callsame` fix for parameterized roles mixed into `$*ARGFILES`,
`use LN` counts lines like rakudo (#15820).
