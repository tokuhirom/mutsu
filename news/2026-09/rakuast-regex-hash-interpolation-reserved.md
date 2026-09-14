# Hash interpolation in regexes remains a reserved boundary

Rakudo currently rejects direct hash interpolation in regexes, such as
`/%var/` and `m/%var/`, with `X::Syntax::Reserved`. A scalar interpolation
whose runtime value is a `Hash` is rejected the same way. mutsu already
matches both boundaries, so the shared `RegexTree` deliberately adds no
hash-specific node and keeps the runtime reservation check intact.

The behavior is pinned by `roast/S05-interpolation/regex-in-variable.t` and
`t/regex/regex-tree-interpolation.t`. If Rakudo later gives hash
interpolation execution semantics, it will require a new source-tree and
execution-lowering slice.
