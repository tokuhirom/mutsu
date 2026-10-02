# Binding a strand string to a variable no longer materializes it

ADR-0120 lets `x` build its result as an unread strand, so
`my str $a = "a" x 2**32 - 1` should never allocate its characters. It did:
every env insert called `as_str()` on the stored value to feed the
sigilless-alias index, before checking whether the key was a sigilless alias
at all, and `as_str()` on a lazy strand string flattens it. The line took
4.3 s and peaked at 4.2 GiB RSS; `t/types/string/str-strands.t` was the
slowest CPU-bound file in `t/` because of it.

The insert now tests the key flag first (the index re-checks the value
itself). The same line runs in ~10 ms and a few MiB, and
`t/types/string/str-strand-bind-not-materialized.t` binds such strings under a
1 GiB `ulimit -v`, so a regression aborts instead of passing slowly.
