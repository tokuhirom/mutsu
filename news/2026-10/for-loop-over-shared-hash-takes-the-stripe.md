# A `for` loop over a shared hash no longer crashes under concurrent inserts

`for %!h { ... }` on an attribute hash that other threads were inserting into
could crash with SIGSEGV in `HashData::typed_key` (#12483). The VM's `for`
dispatch builds a live pair for each key, and it walked the map's keys with
nothing held while another thread rehashed the map. It now walks them under
the container stripe that the method funnel (`.keys`, `.kv`, ...) already
takes. Building the pairs runs no user code, so the stripe is never held
across a callback. In the stress test, 2 of 10 release runs crashed before
the fix and none of 16 afterwards.
