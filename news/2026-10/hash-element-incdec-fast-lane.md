# `%h{$k}++` takes a plain-hash fast lane

`benchmarks/word-count.raku` was 1.45x slower than Raku++ on the bench CI
(0.161 s against 0.111 s), even though its inner loop is one
`%counts{$w}++`. That one operation cost about 6,800 instructions, four times
the ~1,800 of the plain store `%h{$k} = $v`: `exec_inc_dec_index_op` serves
every container shape that can be incremented (QuantHash, object hash,
`is Array`/`is Hash` instances, Buf, Capture, `AT-KEY` overrides, captured
cells, typed and defaulted elements), and it classified the target for all of
them, re-resolving the variable name for each probe, before it reached the
common case.

The four `*IncrementIndex` / `*DecrementIndex` opcodes now try a lane first.
It accepts a plain `%ident` lexical whose element is an `Int` (or absent,
counting from 0), and it reuses the element-store lane's guards, now shared as
`fast_hash_lane_root_is_env` (no cross-thread routing, compunit cell, `our`
mirror or foreign local-slot shape) and `plain_hash_lane_target` (an untyped,
undefaulted, writable hash with no `:=`-bound element). The store and the
increment now also share their commit, `commit_plain_hash_insert`. The lane
never errors: a non-`Int` element, an overflow to a big `Int`, or a guard it
is unsure about declines with nothing touched, and the generic op runs as
before.

Measured on a release build with callgrind, a `%h{$w}++` loop went from 6,783
to 3,400 instructions per iteration (loop overhead included). Locally,
`word-count.raku` went from 0.162 s to about 0.10 s of wall clock, against
Raku++'s 0.111 s on the same machine.
