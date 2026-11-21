# The attribute-hash hammer test asserts rakudo's guarantee, not a stricter one

`t/concurrency/concurrent-attr-hash-writes-do-not-corrupt.t` raced `%!h{$k}++`,
`:delete` and `.keys` on one hash from three threads with no synchronization,
and asserted that the hash stayed intact. About one run in fifteen a worker
panicked instead (`page 0 is the niche` in `nanbox::classify`), because the
read half of `%h{$k}++` walks the map while another thread's write-back
resizes it (#18430).

ADR-12814 makes mutsu's memory-safety guarantee under concurrency rakudo's: a
program that mutates one container from several threads without
synchronization has no defined result. Rakudo 2026.11 dies on every block of
the old test (`MoarVM oops: MVM_str_hash_fetch_nocheck called with a stale
hashtable pointer`, six runs of six), so the panic is case 2.2 of the ADR, not a
mutsu safety bug, and the ADR rules out adding a lock to the read for it.

The test now synchronizes every access with a `Lock`, as Raku asks, which is
the case mutsu must keep memory-safe (guarantee 1). It also checks the exact
results the serialization implies: no `++` is lost on the attribute hash or on
a file-scope hash a method reaches by name. Rakudo passes the new version.
