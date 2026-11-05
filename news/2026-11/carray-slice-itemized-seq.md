# A stored CArray slice keeps its container, and is-deeply compares it

`my $s = $c[1, 3]` on a `CArray` now renders as `$((1.5e0, 4.5e0).Seq)` like
rakudo: resolving the native element references inside a Seq (for `.raku`
and `.gist`) rebuilt the Seq and dropped its `$` container tag. `eqv` (and so
`is-deeply`) also resolved those references before the Seq was reified, so a
deferred `.map` over `CArray` elements never compared equal to the plain
list; it now resolves them after reification too.
