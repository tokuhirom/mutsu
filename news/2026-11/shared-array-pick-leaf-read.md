# `.pick` on an array other threads can reach no longer copies it

Once any thread existed, `@words.pick` copied the whole array on every call. A
read of an array or hash another thread may restructure runs on a snapshot by
default (ADR-0068 §20), and `.pick`/`.roll` were not on the list of reads that
may use the live array under its lock. On Method::Protected's 18278-word list
that was ~400 µs per pick, and its test took 92 s against rakudo's 7 s.

A `.pick`/`.roll` implementation that cannot run user code now reads the live
array under its stripe (ADR-0068 §22), so it is O(1) again: 1-4 µs. The
Callable-count form (`.pick(&count)`), which calls user code, still takes the
snapshot. Method::Protected's t/01 now takes 26 s; the rest is the wrapped
method's call cost (#12875).
