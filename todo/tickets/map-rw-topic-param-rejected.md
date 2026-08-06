# `.map(-> $x is rw { ... })` over an Array rejects the rw topic param

```raku
my @a = 1, 2, 3;
@a.map(-> $x is rw { $x++ });
say @a;   # raku: [2 3 4] — mutsu: X::Parameter::RW: 'x' expects a writable variable argument
```

Raku passes each array element's container to the map block, so an
`is rw` block parameter mutates the element in place. mutsu's map batcher
passes plain element values with no source name and no `ContainerRef`
cell, so the binder's rw check (`X::Parameter::RW` for a non-lvalue
argument) rejects them. Pre-existing, verified on the v0.20.0 release
binary (2026-08-06, while landing shared-cell rw binding — not a
regression from that change). `for @a <-> $x { }` and `.grep` rw views
have their own promotion machinery; the map batcher never got it.

Fix direction: the map/for element loops should pass each element as a
shared element cell (the same leaf-cell shape `deepmap`/hyper already
pass, which the binder's bare-`ContainerRef` arm accepts and binds
writable) — or at least do so when the block signature declares an
`is rw`/`is raw` scalar param. Note ADR-0001: array element cell-ification
(Track B) is fused with the GC campaign, so a full always-celled design
belongs there; a per-call promotion for rw-param blocks is the ticket-size
slice.
