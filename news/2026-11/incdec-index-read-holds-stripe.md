# An indexed `++` reads its element under the write's stripe

`%!h{$k}++` (and `@a[$i]++`) is a read, an increment and a write. The write
half took the ADR-0068 structure stripe for the shared hash, but the read half
walked the same aliased table with nothing held, so another thread's write
could rehash and free it under the lookup. ThreadSanitizer reported it as
`HashMap::get` against `reserve_rehash` in
`t/concurrency/concurrent-attr-array-push-and-kv-iteration.t` (#18667).

The read now takes the same stripe the write keys on (one helper,
`incdec_index_cells`, decides the key for both) and drops it before the
increment, which can run user code. TSan reports for that file went from 16
to 0.
