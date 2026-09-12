# Mixin role native-attribute mutation was already fixed

[#8047](https://github.com/tokuhirom/mutsu/issues/8047) reported that a
role mixed in at runtime (`$obj but R`, `@array does R`) whose method
mutated a native-typed attribute (`has int $.n`) via prefix increment lost
the write on every call — an object whose iteration state lived in a
mixin attribute never advanced, turning an exhaustion loop into an
infinite one.

All three shapes from the issue (a `but`-mixed instance, `does` applied to
an existing `@array` variable, and the reduced `&each`-cursor role with an
`INIT` seed) now measure correctly against `raku` on current `main` —
already fixed, evidently by the ongoing mixin/role attribute-cell work
(see `t/oo/role/mixin-role-attribute-cell.t`, which pins the general
mechanism but not this file's native-int / `does`-on-existing-array /
`INIT`-seed shapes specifically).

Pinned with `t/oo/mixin-typed-int-attr-persists-across-calls.t` so a
regression here is caught even though no `src/` change was needed.
