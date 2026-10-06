# `done` in a `whenever` body completes the enclosing supply

`done` inside a `whenever` of a `supply { ... }` block did not stop the supply:
the rest of the source was still replayed through the body and emitted, so
`supply { whenever Supply.from-list(1, 2, 3) { emit $_; done if $_ == 2 } }`
delivered `1,2,3` where Rakudo delivers `1,2`
([#11999](https://github.com/tokuhirom/mutsu/issues/11999)).

The parser already lowers that `done` to `$emitter.done` followed by a
`SupplyBodyDone` signal, which ends only the whenever body's own closure. The
cold-source replay (`drive_whenever_body_over_values`) never saw the signal, so
it kept feeding values to the body. It now reads the callback's stamped emitter
and stops as soon as the body completed the supply (the emitter's done count
moved), and tells the tap dispatch, which then opens none of the block's
remaining `whenever`s. Plain values the block emitted are still delivered.

Matching Rakudo, the whenever's own `LAST` phaser does not run after such a
`done` (the block closes its subscriptions, it does not see them finish) and
the tap's `done` callback fires once. The same replay serves `.list` and a
derived `.map` over the supply, so both stop at the `done` too. A live
`Supplier` source, a bare `done` in the block body and `done` in a `react`
`whenever` already behaved and are pinned as controls.

Pinned by `t/concurrency/supply/supply-whenever-done-ends-supply.t`, whose
expectations were taken from `raku`.
