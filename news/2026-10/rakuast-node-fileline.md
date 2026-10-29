# `RakuAST::Node.fileline`

`$node.fileline` answers `"<file>:<line>"` where the node's origin begins, and
`Nil` for a node built by hand; the compilation unit itself answers line 1.
Rakudo declares the method (undocumented), and `static::CLI`'s analysis calls
`$cu.fileline` from its CHECK-time closure; the maintainer approved this method
alone on #15531. `use static::CLI` now gets past it and stops at the next
undocumented method, `RakuAST::Origin.as-match` (#15597).
