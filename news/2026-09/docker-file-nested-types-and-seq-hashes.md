# Docker::File reaches parity

Docker::File 1.1 was partial at 1/3 baseline files. Its `generate` tests
exposed two general interpreter gaps: a nested class's own short name in a
method type constraint did not resolve during dispatch, and assigning a `Seq`
of pairs to a `%` attribute left the attribute as a `Seq` instead of building a
`Hash`.

Nested self-type constraints now resolve through the owning class package, and
`%`-sigil constructor coercion materializes `Seq` values with the same pair
handling already used for arrays and slips. Docker::File is now green at 3/3
baseline files and 89/89 assertions.

Pinned by `t/routines/dispatch/nested-class-self-type-dispatch.t` and
`t/oo/attribute/hash-attribute-seq-construction.t`.
