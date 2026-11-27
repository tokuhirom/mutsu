# The binaries pack their relative relocations (DT_RELR)

`mutsu` is a position-independent executable, and it carries about 80,000
relative relocations: every pointer in a vtable, a string table or a static
slice has to be adjusted to where the loader placed the binary. They were
stored as a 1.9 MB `.rela.dyn` table that the dynamic loader read at every
start. After the start-up work mutsu itself did had been cut down, that table
was half of a `say "hello"`'s instructions.

`build.rs` now links the binaries with `-z pack-relative-relocs`, which stores
the same relocations as a DT_RELR bitmap of 24 KB. Nothing is weakened: the
binary is still a PIE with full RELRO and `BIND_NOW`, and every relocation is
still applied; only the table's encoding changes.

The loader must understand DT_RELR, which glibc does from 2.36. A linker does
not check that, so the flag is only passed for a native Linux/glibc build on
a host whose glibc is 2.36 or newer. The release binaries already require
glibc 2.39, so they need nothing newer. `MUTSU_RELR=0` turns it off.

Measured over 300 interleaved runs of `mutsu -e 'say 1'`, the median wall time
drops by 0.3 ms (about 5%) and the minor faults drop from ~978 to ~918.
Refs #19197.
