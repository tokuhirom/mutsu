# Complete shaped-array pairs and zip assignment

Multidimensional shaped arrays now provide writable `.pairs` values by resolving
their tuple keys through the full index path. Native-typed shaped arrays also
accept element-wise `Z=` initialization when assignment normalization leaves the
outer array ordinary but retains shaped rows.

This completes all 43 assertions in `S02-types/array-shapes.t`. The typed-array
coercion keeps the fast behavior of already-shaped declarations; the
100-million-element native-array recursion regression test remains fast.
