# A cyclic Pair renders like rakudo instead of "Too deep recursion"

`my $b = 1; $b = 1 => $b; say $b` builds a Pair whose value container holds the
Pair itself. `gist` and `raku` now track Pair identity in their ancestor stacks
and name the node the walk loops back to, as Rakudo does:
`(\Pair_N = 1 => Pair_N)` for `.gist` and `(my \Pair_N = 1 => Pair_N)` for
`.raku`. The read pre-walk that reifies nested map/grep results also visits a
`ContainerRef` cell once, so it no longer follows such a cycle forever.
