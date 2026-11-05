# `&?ROUTINE` in a block means the routine the block was written in

`&?ROUTINE` was answered at run time from the routine stack, so a block that ran after its
routine returned (the body of a lazy `.map`) or from another routine saw whatever routine was
running then: `sub f { (1, 2).map: -> $i { &?ROUTINE.name } }` gave `(Nil, Nil)` and
`&?ROUTINE(...)` died with "Unknown function". A closure that reads `&?ROUTINE` (directly or
through a nested closure) now binds its enclosing routine when it is created and answers from that
binding, like any other captured lexical. Blocks that read it also take the per-element call path
in `map`/`grep`, instead of the batched inline loop that has no routine frame.

Found with the Deepgrep distribution, whose recursive `deepgrep` calls `&?ROUTINE` from a lazy
`map` block.
