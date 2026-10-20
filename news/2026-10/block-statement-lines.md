# A block's statements report their own lines

An expression block's body (`cap { ... }`, `-> { ... }`, a `.map` block, a
block stored in `&b`) was parsed without per-statement line markers, so every
statement in it reported the line of the statement that built the block. A call
from the block's third line recorded that outer line in its caller frame, and
`callframe(2).line` from a sub called there answered it. BDD::Behave's
`expect` reads the user's line through `callframe` this way and reported the
start of `capture-failures { ... }` instead of the failing `expect` (#13627).

The body now carries a line marker wherever the line changes from the line the
block opens on. Statements on the opening line get none, so a one-line block
keeps the AST shape that the WhateverCode and hash/block checks expect.
