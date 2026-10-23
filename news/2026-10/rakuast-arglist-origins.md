# RakuAST argument lists and empty call nodes carry Rakudo's origins

`.AST` placed the `ArgList` of a parenthesized call over the whole inside of
its parentheses, so `foo( 1 )` and `foo(1 # note\n)` took the spaces and the
comment into it. It now runs from the first argument to the last, a trailing
`,` included, as Rakudo's does, and is left without a range when anything but
whitespace and comments surrounds the arguments.

An argless call (`foo`, `foo()`, `$x.abs`, `self!p`, `1.&foo`) had no
`ArgList` at all, and an empty pointy block (`-> { ... }`) no `Signature`:
mutsu leaves such a field out of the node, as Rakudo's `.raku` does. Rakudo
still fills it with an empty node, which `visit-children` visits and which has
an origin: just inside the parentheses, right after the name, or right after
the arrow. The node now records that origin beside its own, and both the
accessor and `visit-children` build the empty node with it, so equality and
the constructor form are unchanged. A `sub` without a signature is visited
with its empty `Signature` too. `Call::PrivateMethod` and `Call::NameAsMethod`
also get their ranges.
