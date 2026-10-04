# Atomic operations on array and hash elements

The atomic routines now accept an element as their target:
`atomic-fetch(@a[0])`, `atomic-assign(%h<k>, $v)`, `atomic-fetch-inc(@a[$i])`,
`atomic-add-fetch(@a[0], 5)` and the rest of the family (#11812). So do the
`⚛` operators (`@a[0]⚛++`, `++⚛@a[0]`, `⚛@a[0]`) and the integer nqp ops
(`nqp::atomicinc_i(@a[0])`). Before this change, the routine forms died with
"Unknown function: atomic-fetch-inc", and `@a[0]⚛++` silently compiled to a
plain, non-atomic `@a[0]++`.

All of these forms lower to a single helper, `__mutsu_atomic_elem`, which works
on the same element cell that `cas(@a[0], ...)` already swaps. An increment
and a `cas` on the same element therefore take the same lock. Four threads
each doing 250 `@c[0]⚛++` now count to 1000.

Two problems came up along the way and are filed separately:

- an element `cas` or atomic op on an `@`/`%` declared *after* the first
  `start` is lost (#11833);
- the integer atomics still accept a non-native container that Rakudo
  rejects (#11834).
