# Reading a hash that another thread is changing no longer crashes

A new stress test runs 31 kinds of hash reads while other threads insert and delete keys in
the same hash. Nine of them could crash mutsu with a panic, a `double free` abort or a
segmentation fault: `my %c = %x`, `%x{*}`, `.Map`, `.clone`, `%(%x)`, `|%x` in a call, `eqv`
and `.gist`. Each one walked the hash while another thread was rehashing it. These reads now
take the hash's lock (ADR-0068) or work on a copy taken under it. `%h.Hash` still returns the
hash itself.

Concurrent unsynchronized hash access still has no defined answer in Raku. What mutsu
guarantees is that it does not crash. Array readers are not all covered yet; #13079 tracks
the structural fix.
