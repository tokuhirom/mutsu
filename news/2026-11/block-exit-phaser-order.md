# A block's `LEAVE` blocks run before its variables' `will leave` traits; a loop body's exit phasers are last-declared-first

Two ordering fixes for exit phasers, both measured against rakudo 2026.09:

- In a bare block, a sub or a loop body, every `LEAVE { }` block runs first (last
  declared first) and then the variables' `will leave` / `will keep` / `will undo`
  traits (last declared first). mutsu interleaved them in one reverse declaration
  order (`body L3 b L2 a L1` where rakudo prints `body L3 L2 L1 b a`). The
  `SyntheticBlock` flattening in `reorder_recursive` now moves a trait's exit
  phasers to the front of the level's list, which the block compiler queues from
  the back, the same placement the main program got in #16613.
- A loop body's own `LEAVE` / `KEEP` / `UNDO` blocks ran in declaration order on a
  normal iteration (`for 1 { LEAVE { say 1 }; LEAVE { say 2 } }` printed `1 2`,
  rakudo `2 1`), although the early-exit path (`last`/`next`) was already
  last-declared-first. Both paths now share the reversed order.

Closes #16606. Found by the raku-doc differential (`Language/phasers.rakudoc`).
The spurious "Useless use" warning on a `for` body whose tail feeds a KEEP is #16616.
