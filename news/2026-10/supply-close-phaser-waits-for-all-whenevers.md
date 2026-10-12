# A supply block's CLOSE waits for all of its `whenever`s

`Supply.tap` registered a `__SupplyCloseMarker` on every supplier-backed `whenever`
source of a `supply { }` block. The first source to finish therefore ran the block's
CLOSE phasers, while the block was still subscribed to its other sources:
`supply { whenever Promise.in(0.1) { }; whenever $s.Supply { }; CLOSE ... }` closed
after 0.1 s.

The per-source marker is gone. The CLOSE marker is now the terminal callback of the
block's done group, which fires once every counted `whenever` has finished. It runs after
the downstream `done`, as in rakudo. The group is created even for a tap with no
`done =>`, so such a block still runs CLOSE when it ends. An explicit `Tap.close` or
`done` takes the same callbacks, so CLOSE runs once whichever comes first. (#12880)
