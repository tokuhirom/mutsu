# A block's `sub` no longer rescans the routine registry per iteration

`for ^N { { sub g { 1 }; g() } }` cost O(R) per iteration, R = registered
routines (#9170). Each iteration re-installs `g` and dirties its entry in the
base-name key index. The index can refill a dirty name from the qualified-name
tail index, which costs O(k) for the name's k keys. It did that only while it
had never seen an unqualified key. A block sub's eviction names its unqualified
spelling, so every refill fell back to a scan of the whole functions map.

The index now records the unqualified keys it sees, per base name, and adds
them to what the tail index lists. A dirty name's refill is O(k) again, however
many unqualified keys other names have.
