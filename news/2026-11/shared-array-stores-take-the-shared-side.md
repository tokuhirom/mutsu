# Stores to disjoint slots of a shared array stop excluding each other

`bench-threads`' write section has four workers, each storing into its own 16
slots of one `:=`-bound array. It was the last part of #8069. Every store held
ADR-0068's cell and node stripes exclusively, so four writers handed the locks
to one another 80k times and parked on a futex each time.

ADR-18752 D1 and D2 land here:

- The stripes are reader/writer locks now. Every existing holder (structural
  writes and live-node reads) takes the exclusive side, exactly as before.
- An in-range store of a refcount-free word (a small `Int`, a `Num`, a
  `Bool`, a type object) into a plain celled `Array` takes the same two stripes
  on the shared side and compare-and-swaps the slot word. Because both the old
  and the new word own nothing, no reference moves through the swap and nothing
  has to be dropped or retired.
- Any other store keeps the exclusive path.

On a release build in the 4-core container, the write section went from
265-371 ms to 47-59 ms. The whole `bench-threads` went from 373-453 ms to
117-139 ms, against raku's 425-696 ms, with the same checksum. The single-thread
serial benchmark is unchanged. D4 (reaching the node without the cell mutex) was
not needed to clear the ADR's 150 ms gate.
