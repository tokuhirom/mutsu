# `Map.ASSIGN-KEY` refuses an immutable Map

`Map.new((a => 1)).ASSIGN-KEY('b', 2)` stored the key through the `Hash` row. It now dies with
Rakudo's `Cannot add key 'b' to an immutable Map` (`Cannot change key 'a' in an immutable Map`
for an existing key). The remaining names `.^methods` does not list for `Map`/`Hash`/`Array`
(`from-slurpy*`, `set-shape`, `is-generic`, `STORE_AT_KEY`, `INSTANTIATE-GENERIC`) are
Rakudo-internal hooks with no row; the introspection lists only what a row reaches, so they stay
unlisted by design.

Part of #11276; closes #14134. Pinned in `t/collections/transform/map-assign-key-immutable.t`.
