# Reifying a shared hash or array no longer races concurrent inserts

`reify_nested_map_grep_for_read` cloned the values of a shared `has %!h` (and walked a shared array) with no container stripe held, so a concurrent insert that reallocated the backing store could hand the reader freed memory (#12631). The element clone now happens under the stripe, and the recursion into user-visible reification runs with nothing held. The #12456 pin gained an array case.
