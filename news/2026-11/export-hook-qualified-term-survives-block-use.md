# A qualified term from a `sub EXPORT` hook survives a block-scoped `use`

A `sub EXPORT` hook that hands out a package head and a term qualified under it
(`'Graph' => Graph, 'Graph::Cycle' => Graph::Cycle`) made the program's own
`Graph::Cycle` read as `Any` once a block `use`d the same module. The block
declares both names as terms, and writing the qualified term key (`\Graph::Cycle`)
was taken for a write to a package variable reached through a `constant` alias of
the package (the head term `\Graph` is bound to the package). The write landed
on `Graph::Cycle` instead, the term key kept its declaration seed, and the block's
exit restored that seed into the program's slot. A term key is never a package
variable now, so the write stays on the term (#16283, found with Graph::RandomMaze).
