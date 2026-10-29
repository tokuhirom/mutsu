# A built-in class name no longer scans every registry class to say "not here"

`resolved_class_parents` falls back to a class declared as `Outer::Name` when a bare
`Name` has no registry entry of its own. Every built-in type name (`Int`, `Any`,
`Mu`) has none, so each parent-chain step of `.can`, `.^mro` and the grammar /
container-subclass checks split all ~400 registry keys to discover the miss:
~400K instructions for one `Int.can("x")`. The registry now keeps a
`short name -> class key` index as a `OnceLock` dropped by the registry write
guard's `DerefMut` (the same mechanism as the lexical-type-name index), and the
answer is one probe: `Int.can("nonesuch")` 398,787 -> 59,969 instructions. When two
classes share a tail the smallest key wins, so the answer no longer depends on hash
map order. Refs #15498.
