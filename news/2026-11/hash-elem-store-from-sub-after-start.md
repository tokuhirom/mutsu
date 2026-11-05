# Element stores from a sub survive a spawned `start` block

Once a `start` block had been spawned, `%h{$k} = $v` (and `@a[$i] = $v`) done from
a named sub or method on an outer file-scope or `our` container was silently
lost until the promise was awaited ([#16221](https://github.com/tokuhirom/mutsu/issues/16221)).

Two separate causes:

- The element-store wrapper seeds `env` with a unit-lexical cell's contents and
  stores the `env` value back into the cell afterwards. The cross-thread atomic
  lane (`shared_hash_elem_set`) already wrote the result into the cell, so the
  stale seeded copy overwrote it. The wrapper now skips the write-back when the
  cell changed and `env` still holds the seed.
- An `our %h` / `our @a` is read from the package mirror, which the name-keyed
  shared lane never updated. The lane commit now declines for such names so the
  ordinary path writes the mirror.

The three duplicated "write the lane result back" branches share
`store_shared_lane_result`. Regression test:
`t/concurrency/concurrent-hash-elem-store-from-sub-after-start.t`.
