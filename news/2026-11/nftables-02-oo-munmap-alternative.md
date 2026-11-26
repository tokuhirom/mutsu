# Linux::NFTables t/02-oo's accepted crash gains glibc's munmap_chunk wording

`Linux::NFTables` `t/02-oo.rakutest` is an accepted divergence (#17834). Every assertion passes,
then the class's `DESTROY` frees a libnftables context that the library itself double-frees.
The resulting heap corruption surfaces as a different glibc message on each run, so the entry
lists the alternatives. The 2026-11-26 sweep hit one more of them,
`munmap_chunk(): invalid pointer`, with the same `ok = 2` shape. It is now in the list.
