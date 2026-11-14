# A multi-dimensional read holds the container's structure lock

`roast/S17-lowlevel/cas-int.t` died now and then with `Cannot unbox a type
object (Nil) to int` (#17781). A reduced loop reproduced the death in about
three runs out of five on a debug build. Each pass redeclares
`my atomicint @values[2;2]` and has four `start` workers run
`cas(@values[1;1], ...)`. The same loop also turned up garbage `Num`s and an
element written to the wrong column. Those two point to a use-after-free, not
just a stale read.

The multi-dimensional subscript reader went down each level by borrowing the
live element (`&items[i]`), and it took no ADR-0068 structure stripe while it
did so. Another worker's first `cas` promotes the row `@values[1]` to a shared
element cell, in place. Later `cas` calls re-seat that cell's row and drop the
old one. A reader that was part-way through the old slot could see it half
promoted, or walk a row that had already been freed.

The single-dimension read already holds the node's stripe. Now the
multi-dimensional walk does too:

- Each level clones the element it is about to descend into while holding the
  container's stripe. This covers a scalar index, an index list, `*`, and the
  bounds check of a shaped array.
- The next level therefore receives an owned value with its own reference
  count, not a borrow.
- Hash levels clone their values the same way.
- The stripe is held only around the clone. A `WhateverCode` dimension still
  runs with no lock held.

Element promotion (`array_slot_ref`, the hash promotions) also builds the new
cell from a copy and publishes it with a single store. Previously it parked
`Nil` in the slot while the cell was being built.

After the fix, the reduced loop and the roast file each passed 18 times in a
row while running alongside each other.
